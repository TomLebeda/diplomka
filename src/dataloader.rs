use std::{
    collections::HashMap,
    fmt::Display,
    path::{Path, PathBuf},
    usize,
};

use itertools::Itertools;
use log::*;
use serde::{Deserialize, Serialize};

use crate::{errors::SceneError, parser::ParseNode, utils::remove_number_from_obj};

/// Represents configuration of loss values
#[derive(Debug, Serialize, Deserialize, Default)]
pub struct LossTable {
    /// default loss value for all missing objects
    pub missing_objects: f32,
    /// default loss value for all missing attributes
    pub missing_attributes: f32,
    /// default loss value for all missing triplets
    pub missing_triplets: f32,
    /// additional loss for when object is present, but is missing number
    /// will apply to all loss values as additional value
    pub numberless_penalty: f32,
    /// default loss value for all wrong values on attributes
    pub wrong_values: f32,
    /// override loss values for specific missing objects (by tag)
    pub missing_objects_override: Vec<(String, f32)>,
    /// override loss values for specific missing attributes (by attribute)
    pub missing_attributes_override: Vec<(String, f32)>,
    /// override loss values for specific missing triplets (by predicate)
    pub missing_triplets_override: Vec<(String, f32)>,
    /// override loss values for specific wrong attribute values (by attribute)
    pub wrong_values_override: Vec<WrongValueOverride>,
}

impl LossTable {
    /// Returns the loss value for missing object, while respecting priority of loss values.
    pub fn get_loss_missing_obj(&self, obj: &SceneObject) -> f32 {
        // find first tag-loss value
        for tag in &obj.tags {
            if let Some((_, loss)) = self
                .missing_objects_override
                .iter()
                .find(|(t, _)| return t == tag)
            {
                return *loss;
            }
        }
        // if there is none, use the default loss
        return self.missing_objects;
    }

    /// Returns the loss value for missing attribute, while respecting priority of loss values.
    pub fn get_loss_missing_attr(&self, attr: &str) -> f32 {
        // try to find attribute-specific loss value for missing attribute
        if let Some((_, loss)) = self
            .missing_attributes_override
            .iter()
            .find(|(a, _)| return a == attr)
        {
            return *loss;
        }
        // otherwise return the default value
        return self.missing_attributes;
    }

    /// Returns the loss value for missing triplet, while respecting priority of loss values.
    pub fn get_loss_missing_triplet(&self, predicate: &str) -> f32 {
        // try to find predicate-specific loss value for missing attribute
        if let Some((_, loss)) = self
            .missing_triplets_override
            .iter()
            .find(|(pred, _)| return pred == predicate)
        {
            return *loss;
        }
        // otherwise return the default value
        return self.missing_triplets;
    }

    /// Returns the loss value for wrong value, while respecting priority of loss values.
    pub fn get_loss_wrong_value(
        &self,
        value: &String,
        target_value: &String,
        attribute: &str,
    ) -> f32 {
        if let Some(value_override) = self
            .wrong_values_override
            .iter()
            .find(|o| return o.attribute == attribute)
        {
            for (values, loss) in &value_override.overrides {
                if values.contains(value) && values.contains(target_value) {
                    return *loss;
                }
            }
            return value_override.default;
        }
        return self.wrong_values;
    }
}

/// Sub-type that specifies how to override loss values for specific attribute
#[derive(Debug, Serialize, Deserialize)]
pub struct WrongValueOverride {
    /// the attribute that this override applies to
    pub attribute: String,
    /// default loss value for the attribute
    pub default: f32,
    /// list of sets of values that have each own
    pub overrides: Vec<(Vec<String>, f32)>,
}

/// Represents final evaluation score computed from extracted data, scene and loss table.
#[derive(Serialize, Deserialize, Default, Debug)]
pub struct Score {
    /// accumulated score of all missing objects
    pub missing_objects: f32,
    /// accumulated score of all missing attributes
    pub missing_attributes: f32,
    /// accumulated score of all missing triplets
    pub missing_triplets: f32,
    /// accumulated score of all wrong attribute values
    pub wrong_values: f32,
    /// accumulated score of missing objects, but grouped by each tag
    pub grouped_missing_objects: HashMap<String, f32>,
    /// accumulated score of missing objects, but grouped by each attribute
    pub grouped_missing_attributes: HashMap<String, f32>,
    /// accumulated score of missing objects, but grouped by each predicate
    pub grouped_missing_triplets: HashMap<String, f32>,
    /// accumulated score of wrong values, but grouped by each attribute
    pub grouped_wrong_values: HashMap<String, f32>,
}

/// A piece of semantic information extracted from a text using provided scene and grammar
#[derive(Serialize, Deserialize, Debug)]
pub struct Extract {
    /// number of seconds from the beginning of the speech at which this piece of semantic
    /// information was obtained
    pub timestamp: Option<u32>,
    /// the source sentence from which this piece of semantic information was extracted
    pub sentence: String,
    /// the actual data that was extracted
    pub data: ExtractData,
    /// the original parse tree in a form of root (ParseNode)
    pub tree: ParseNode,
}

/// Defines what type of information was extracted
#[derive(Serialize, Deserialize, Debug)]
pub enum ExtractData {
    /// the extracted semantic information is just some object name
    Object(String),
    /// the extracted semantic information is a triplet describing some relation between two objects
    Triplet(Triplet),
    /// the extracted semantic information is a attribute describing some static property of an object
    Attribute(Attribute),
}

impl Display for ExtractData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExtractData::Object(o) => return write!(f, "Object {{{o}}}"),
            ExtractData::Triplet(t) => return write!(f, "Triplet {{{t}}}"),
            ExtractData::Attribute(a) => return write!(f, "Attribute {{{a}}}"),
        }
    }
}

/// Represents the output of 'prepare' CLI command
#[derive(Serialize, Deserialize, Debug)]
pub struct Checkpoint {
    /// mapping of words that are considered objects in a scene
    /// the mapping is [single en-word] -> [many cz-words]
    pub objects: HashMap<String, Vec<String>>,
    /// mapping of words that are static attributes in a scene
    /// the mapping is [single en-word] -> [many cz-words]
    /// for attribute of name "foo" the keys will have "foo_VALS" format
    pub attributes: HashMap<String, HashMap<String, Vec<String>>>,
    /// map of words that are predicates and require hand-crafted patterns
    /// predicates include dynamic relations as well as static attributes
    /// keys are names of the predicates, while the values are templates that will be used for
    /// generation of the triplet, replacing:
    ///     1. {OBJ=object} with {obj_start} $objects {obj_end}
    ///     2. {SUBJ=attr_name_VALS} with {subj_start} $attr_name_VALS {subj_end}
    pub predicates: HashMap<String, String>,
}

#[derive(Debug, Serialize, Deserialize)]
/// Representation of the scene that is shown in some image.
/// It consists of objects with attributes and their connections (triplets)
pub struct Scene {
    /// width of the underlying image in pixels
    width: u32,
    /// height of the underlying image in pixels
    height: u32,
    /// path to the underlying image file
    image_path: String,
    /// list of objects that are in the scene
    objects: Vec<SceneObject>,
    /// list of the triplets that describe connections between the objects
    triplets: Vec<Triplet>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
/// Representation of the objects withing a scene.
pub struct SceneObject {
    /// name of the object, must be unique (serves as ID)
    pub name: String,
    /// list of tags that are associated with the object
    pub tags: Vec<String>,
    /// coordinates (x, y) of top left corner of bounding box of the object, in pixels counted from top-left corner of the image
    pub top_left_corner: (u32, u32),
    /// size (width, height) of the bounding box in pixels
    pub size: (u32, u32),
    /// names of parent objects, empty if this object doesn't have any parent objects
    pub parents: Vec<String>,
    /// names of child objects, empty vec if this object doesn't consist of other objects.
    pub children: Vec<String>,
    /// list of attributes (key, value) of this object
    pub attributes: Vec<(String, String)>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord)]
/// represents some attribute (property) of some [SceneObject]
pub struct Attribute {
    /// name of the object
    pub object: String,
    /// name of the attribute
    pub attribute: String,
    /// value of the attribute
    pub value: String,
}

impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(
            f,
            "{}: {} = {}",
            self.object.replace(' ', "_"),
            self.attribute.replace(' ', "_"),
            self.value.replace(' ', "_")
        );
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord)]
/// represents triplet 'from --predicate--> to'
pub struct Triplet {
    /// source node
    pub from: String,
    /// predicate (connection label)
    pub predicate: String,
    /// target node
    pub to: String,
}

impl Display for Triplet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(
            f,
            "{} {} {}",
            self.from.replace(' ', "_"),
            self.predicate.replace(' ', "_"),
            self.to.replace(' ', "_")
        );
    }
}

impl SceneObject {
    /// Returns copy of list of children object names
    pub fn get_children(&self) -> Vec<String> {
        return self.children.clone();
    }
    /// Returns copy of list of parent object names
    pub fn get_parents(&self) -> Vec<String> {
        return self.parents.clone();
    }
    /// Returns copy of list of tags
    pub fn get_tags(&self) -> Vec<String> {
        return self.tags.clone();
    }
    /// Returns a list of all attribute names that are in this object.
    pub fn get_attr_names(&self) -> Vec<String> {
        return self
            .attributes
            .iter()
            .map(|(attr_name, _attr_val)| return attr_name.clone())
            .collect_vec();
    }
    /// Returns a tuple (x, y) with coordinates of the center point
    pub fn get_center(&self) -> (u32, u32) {
        let top_left = self.get_bbox_top_left_corner();
        let size = self.get_bbox_size();
        return (top_left.0 + size.0 / 2, top_left.1 + size.1 / 2);
    }
    /// Returns a list of all attribute names that are in this object.
    pub fn get_attribute_values(&self) -> Vec<String> {
        return self
            .attributes
            .iter()
            .map(|(_attr_name, attr_val)| return attr_val.clone())
            .collect_vec();
    }

    /// Returns all unique attributes (key, value) withing this object (cloned)
    pub fn get_attributes(&self) -> Vec<Attribute> {
        return self
            .attributes
            .iter()
            .map(|(attr_name, attr_val)| {
                return Attribute {
                    object: self.name.to_string(),
                    attribute: attr_name.clone(),
                    value: attr_val.clone(),
                };
            })
            .collect_vec();
    }

    /// Returns the name of the object, which serves as its ID.
    /// Names must be unique in their scenes.
    pub fn get_name(&self) -> &str {
        return &self.name;
    }

    /// Returns coordinates (x, y) of the top left corner of the bounding box.
    /// The unit is pixels of the original image with origin being top-left corner.
    pub fn get_bbox_top_left_corner(&self) -> (u32, u32) {
        return self.top_left_corner;
    }

    /// Returns the size (w, h) of bounding box of the object.
    /// The unit is pixels of the original image.
    pub fn get_bbox_size(&self) -> (u32, u32) {
        return self.size;
    }

    /// Transforms the object into list of triplets that completely describe the object.
    pub fn crumble(&self) -> Vec<Triplet> {
        let mut triplets: Vec<Triplet> = vec![
            Triplet {
                from: self.name.clone(),
                predicate: String::from("x min"),
                to: self.top_left_corner.0.to_string(),
            },
            Triplet {
                from: self.name.clone(),
                predicate: String::from("y min"),
                to: self.top_left_corner.1.to_string(),
            },
            Triplet {
                from: self.name.clone(),
                predicate: String::from("x max"),
                to: self.top_left_corner.0.to_string(),
            },
            Triplet {
                from: self.name.clone(),
                predicate: String::from("y max"),
                to: self.top_left_corner.0.to_string(),
            },
        ];
        self.attributes.iter().for_each(|(k, v)| {
            triplets.push(Triplet {
                from: self.name.clone(),
                predicate: k.clone(),
                to: v.clone(),
            })
        });
        self.parents.iter().for_each(|parent| {
            triplets.push(Triplet {
                from: self.name.clone(),
                predicate: String::from("has parent object"),
                to: parent.clone(),
            })
        });
        self.children.iter().for_each(|c| {
            triplets.push(Triplet {
                from: self.name.clone(),
                predicate: String::from("has child object"),
                to: c.clone(),
            })
        });
        return triplets;
    }
}

impl Scene {
    /// Returns true if the scene contains some object with given name, otherwise false.
    /// If `ignore_numbers` is true, then the object numbering will be ignored, so "foo" will match "foo #1"
    pub fn contains_object(&self, obj_name: &str, ignore_numbers: bool) -> bool {
        for object in &self.objects {
            if ignore_numbers {
                if obj_name == remove_number_from_obj(&object.name) {
                    return true;
                }
            } else {
                if obj_name == object.name {
                    return true;
                }
            }
        }
        return false;
    }

    /// Returns true if the scene contains some object with given name, otherwise false.
    /// If `ignore_numbers` is true, then the object numbering will be ignored, so "foo" will match "foo #1"
    /// If `ignore_value` is true, then the value doesn't need to match as well
    pub fn contains_attribute(
        &self,
        attr: &Attribute,
        ignore_value: bool,
        ignore_numbers: bool,
    ) -> bool {
        for object in &self.objects {
            let name = match ignore_numbers {
                true => remove_number_from_obj(&object.name),
                false => &object.name,
            };
            let name_matches = attr.object == name;
            if !name_matches {
                continue;
            }
            let attribute_exist = object.get_attr_names().contains(&attr.attribute);
            if !attribute_exist {
                continue;
            }
            if ignore_value {
                if name_matches && attribute_exist {
                    return true;
                }
            } else {
                let (_, attr_value) = object
                    .attributes
                    .iter()
                    .find(|(name, _)| return name == &attr.attribute)
                    .unwrap(); // should be safe because we checked if attribute exist
                let value_matches = &attr.value == attr_value;
                // obj_name and attr_name matches must have already passed by now
                if value_matches {
                    return true;
                }
            }
        }
        return false;
    }

    /// Returns true if the scene contains some object with given name, otherwise false.
    /// Hierarchy between objects is considered a special case of triplets, it is handled here as well.
    /// For the hierarchy case, the `ignore_numbers` toggle is used
    pub fn contains_triplet(&self, triplet: &Triplet, ignore_numbers: bool) -> bool {
        if triplet.predicate == "has child object" {
            let Some(parent) = self.get_object(&triplet.from) else {
                return false;
            };
            let child_names = match ignore_numbers {
                false => parent.get_children(),
                true => parent
                    .children
                    .iter()
                    .map(|name| return remove_number_from_obj(name).to_string())
                    .collect_vec(),
            };
            return child_names.contains(&triplet.to);
        } else if triplet.predicate == "has parent object" {
            let Some(child) = self.get_object(&triplet.from) else {
                return false;
            };
            let parent_names = match ignore_numbers {
                false => child.get_children(),
                true => child
                    .parents
                    .iter()
                    .map(|name| return remove_number_from_obj(name).to_string())
                    .collect_vec(),
            };
            return parent_names.contains(&triplet.to);
        } else {
            if ignore_numbers {
                return self.triplets.iter().any(|t| {
                    let from_matches =
                        remove_number_from_obj(&t.from) == remove_number_from_obj(&triplet.from);
                    let to_matches =
                        remove_number_from_obj(&t.to) == remove_number_from_obj(&triplet.to);
                    let predicate_matches = t.predicate == triplet.predicate;
                    return from_matches && to_matches && predicate_matches;
                });
            } else {
                return self.triplets.contains(triplet);
            }
        }
    }

    /// Creates triplet-only representation from the scene.
    /// All objects and their attributes will be transformed into triplets as well as the hierarchy.
    pub fn crumble(&self) -> Vec<Triplet> {
        let mut triplets: Vec<Triplet> = vec![];
        for object in &self.objects {
            triplets.append(&mut object.crumble())
        }
        triplets.append(&mut self.triplets.clone());
        return triplets;
    }

    /// Returns all attributes present in all objects of the scene.
    pub fn get_attributes(&self) -> Vec<Attribute> {
        return self
            .objects
            .iter()
            .flat_map(|obj| return obj.get_attributes())
            .collect_vec();
    }

    /// Returns the name of the image that is assigned to the scene.
    /// Splits the attached file path at "/" and returns the last part or full name if there is nothing to split
    pub fn get_image_name(&self) -> String {
        let file_name = match self.image_path.rsplit_once('/') {
            Some((_prefix, suffix)) => suffix,
            None => self.image_path.as_str(),
        };
        // remove the .jpg or whatever from the name
        match file_name.split_once('.') {
            Some((prefix, _suffix)) => return prefix.to_string(),
            None => return file_name.to_string(),
        }
    }

    /// Returns a list of all unique predicates, sorted alphabetically in ascending order.
    pub fn get_predicates(&self) -> Vec<String> {
        return self
            .triplets
            .iter()
            .map(|triplet| return triplet.predicate.clone())
            .unique()
            .sorted()
            .collect_vec();
    }
    /// Returns a list of all object tags that are used in the scene.
    /// Values are sorted alphabetically and unique.
    pub fn get_tags(&self) -> Vec<String> {
        return self
            .objects
            .iter()
            .flat_map(|obj| return obj.get_tags())
            .unique()
            .sorted()
            .collect_vec();
    }

    /// Returns a list of all unique attribute names in all objects in the scene,
    /// sorted alphabetically in ascending order.
    pub fn get_attribute_names(&self) -> Vec<String> {
        return self
            .objects
            .iter()
            .flat_map(|obj| return obj.get_attr_names())
            .unique()
            .sorted()
            .collect_vec();
    }

    /// Returns a list of all unique attribute values in all objects in the scene,
    /// sorted alphabetically in ascending order.
    pub fn get_attribute_values(&self) -> Vec<String> {
        return self
            .objects
            .iter()
            .flat_map(|obj| return obj.get_attribute_values())
            .unique()
            .sorted()
            .collect_vec();
    }

    /// Returns a list of names of all objects in the scene, sorted alphabetically in ascending order.
    pub fn get_object_names(&self) -> Vec<String> {
        return self
            .objects
            .iter()
            .map(|obj| return obj.name.clone())
            .sorted()
            .collect_vec();
    }

    /// Returns a reference to vector of all SceneObjects in the scene
    pub fn get_all_objects(&self) -> &Vec<SceneObject> {
        return &self.objects;
    }

    /// Returns a reference to vector of all Triplets in the scene (without crumbling)
    pub fn get_all_triplets(&self) -> &Vec<Triplet> {
        return &self.triplets;
    }

    /// Returns a cloned vector of all Triplets in the scene (without crumbling, but with hierarchy included)
    pub fn get_all_triplets_with_hierarchy(&self) -> Vec<Triplet> {
        let mut triplets = self.triplets.clone();
        for obj in &self.objects {
            obj.parents.iter().for_each(|parent| {
                triplets.push(Triplet {
                    from: obj.name.clone(),
                    predicate: String::from("has parent object"),
                    to: parent.clone(),
                })
            });
            obj.children.iter().for_each(|c| {
                triplets.push(Triplet {
                    from: obj.name.clone(),
                    predicate: String::from("has child object"),
                    to: c.clone(),
                })
            });
        }
        return triplets;
    }

    /// Searches for object by name and returns it.
    pub fn get_object(&self, name: &str) -> Option<&SceneObject> {
        return self.objects.iter().find(|&obj| return obj.name == name);
    }

    /// Returns dimensions of the associated image as a tuple (width, height) in pixels.
    pub fn get_image_size(&self) -> (u32, u32) {
        return (self.width, self.height);
    }

    /// Returns the provided path of the image file associated with the scene
    pub fn get_image_path(&self) -> PathBuf {
        return PathBuf::from(self.image_path.clone());
    }

    /// Returns the number of objects that are defined withing the scene.
    pub fn get_object_count(&self) -> usize {
        return self.objects.len();
    }

    /// Returns the number of triplets that are defined withing the scene.
    /// Triplets that can be generated by crumbling does not count.
    pub fn get_triplet_count(&self) -> usize {
        return self.triplets.len();
    }

    /// Loads scene from JSON file.
    /// Returns error when the file is not found or is unreadable
    pub fn from_file(path: &Path) -> Result<Scene, String> {
        let Ok(raw_str) = std::fs::read_to_string(path) else {
            return Err(format!(
                "can't find or read file \"{}\"",
                path.to_string_lossy()
            ));
        };
        trace!("file \"{}\" succesfully loaded.", path.to_string_lossy());

        let scene: Scene = match serde_json::from_str(&raw_str) {
            Ok(scene) => scene,
            Err(e) => {
                return Err(format!(
                    "can't parse file \"{}\": {}",
                    path.to_string_lossy(),
                    e
                ));
            }
        };
        trace!(
            "file \"{}\" succesfully parsed into Scene",
            path.to_string_lossy()
        );

        return Ok(scene);
    }

    /// Checks the [Scene] for:
    ///     1. names of objects must be unique
    ///     2. coordinates of the objects must be valid (not out of bounds)
    ///     3. references to other objects (child/parent) must exist and be both ways
    ///
    /// Returns found issues as vector of [SceneError] or empty vector if everything is okay.
    pub fn check(&self) -> Vec<SceneError> {
        let mut errs: Vec<SceneError> = vec![];
        if let Some(e) = self.check_image() {
            errs.push(e)
        }
        errs.append(&mut self.check_duplicit_names());
        errs.append(&mut self.check_coordinates());
        errs.append(&mut self.check_object_links());
        errs.append(&mut self.check_triplets());
        return errs;
    }

    /// Checks if the triplets refers to existing objects.
    fn check_triplets(&self) -> Vec<SceneError> {
        trace!("checking triplets...");
        let mut errs = self
            .triplets
            .iter()
            .filter_map(|triplet| {
                if self.get_object(&triplet.from).is_none() {
                    return Some(SceneError::TripletFromNotFound {
                        triplet: triplet.clone(),
                    });
                }
                return None;
            })
            .collect_vec();
        errs.append(
            &mut self
                .triplets
                .iter()
                .filter_map(|triplet| {
                    if self.get_object(&triplet.to).is_none() {
                        return Some(SceneError::TripletToNotFound {
                            triplet: triplet.clone(),
                        });
                    }
                    return None;
                })
                .collect_vec(),
        );
        return errs;
    }

    /// Checks if the image associated with this scene exists on given path.
    fn check_image(&self) -> Option<SceneError> {
        trace!("checking image file...");
        if !Path::new(&self.image_path)
            .try_exists()
            .is_ok_and(|exists| return exists)
        {
            trace!("image file was not found or is unreadable");
            return Some(SceneError::ImageNotFound {
                path: self.image_path.clone(),
            });
        }
        trace!("OK: image file is readable");
        return None;
    }

    /// Checks if all objects have valid links, which means:
    ///     1. object must not reference itself
    ///     2. referenced objects must exist
    ///     3. parent-child links must be both-ways
    /// Returns a list of [SceneError] or empty list of no problems are found.
    fn check_object_links(&self) -> Vec<SceneError> {
        trace!("checking for reference problems...");
        let mut errs: Vec<SceneError> = vec![];
        let object_names = &self
            .objects
            .iter()
            .unique_by(|obj| return &obj.name)
            .map(|obj| return &obj.name)
            .collect_vec();
        self.objects.iter().for_each(|obj| {
            // check the parent objects
            obj.parents.iter().for_each(|parent_name| {
                // 1. check if it is self-reference
                if &obj.name == parent_name {
                    errs.push(SceneError::SelfReference {
                        name: obj.name.clone(),
                    })
                }
                // 2. check if it is non-existing reference
                if !object_names.contains(&&obj.name) {
                    errs.push(SceneError::ParentNotFound {
                        child: obj.name.clone(),
                        parent: parent_name.clone(),
                    })
                }
                // 3. check if parent objects have this object listed as a child
                self.objects
                    .iter()
                    .filter(|o| return &o.name == parent_name)
                    .for_each(|parent| {
                        if !parent.children.contains(&obj.name) {
                            errs.push(SceneError::MissingChild {
                                child: obj.name.clone(),
                                parent: parent_name.clone(),
                            })
                        }
                    });
            });

            // and now the reverse for children:
            for child_name in &obj.children {
                // 1. check if it is self-reference
                if &obj.name == child_name {
                    errs.push(SceneError::SelfReference {
                        name: child_name.clone(),
                    })
                }
                // 2. check if the referenced objects exist
                if !object_names.contains(&child_name) {
                    errs.push(SceneError::ChildNotFound {
                        child: child_name.clone(),
                        parent: obj.name.clone(),
                    })
                }
                // 3. check if child objects have this object listed as a parent
                self.objects
                    .iter()
                    .filter(|o| return &o.name == child_name)
                    .for_each(|child| {
                        if !child.parents.contains(&obj.name) {
                            errs.push(SceneError::MissingParent {
                                parent: obj.name.clone(),
                                child: child_name.clone(),
                            })
                        }
                    });
            }
        });
        match errs.len() {
            0 => trace!("OK: all references are valid"),
            n => trace!("found {} problems with referencing", n),
        }
        return errs;
    }

    /// Checks if all objects have bounding boxes inside the actual image.
    /// Returns a vector of [SceneError::BboxOutOfBounds] or empty vector if no outlayers are found.
    fn check_coordinates(&self) -> Vec<SceneError> {
        trace!("checking for bounding box outlayers...");
        let max_x = self.width;
        let max_y = self.height;
        let outlayers = self
            .objects
            .iter()
            .filter(|obj| {
                let obj_x = obj.top_left_corner.0 + obj.size.0;
                let obj_y = obj.top_left_corner.1 + obj.size.1;
                let is_out_of_bounds = obj_x > max_x || obj_y > max_y;
                return is_out_of_bounds;
            })
            .map(|obj| {
                return SceneError::BboxOutOfBounds {
                    object: obj.clone(),
                    scene_size: (max_x, max_y),
                };
            })
            .collect_vec();
        match outlayers.len() {
            0 => trace!("OK: no outlayers were found"),
            n => trace!("found {} outlayers", n),
        };
        return outlayers;
    }

    /// Checks if all objects in a given Scene have unique names.
    /// Returns vector of [SceneError::DuplicitName] or empty list if no duplicates are found.
    fn check_duplicit_names(&self) -> Vec<SceneError> {
        trace!("checking for name duplicates...");
        let mut errs: Vec<SceneError> = vec![];
        let name_map = &self.objects.iter().counts_by(|obj| return &obj.name);
        for (key, val) in name_map {
            if *val > 1 {
                errs.push(SceneError::DuplicitName {
                    name: key.to_string(),
                    number_of_occurences: *val as u32,
                })
            }
        }
        match errs.len() {
            0 => trace!("OK: no duplicates found."),
            n => trace!("found {} duplicate(s)", n),
        }
        return errs;
    }

    /// Returns all attributes in the scene grouped by key
    pub fn get_attributes_grouped(&self) -> Vec<(String, Vec<String>)> {
        let attributes = self.get_attributes();
        let mut map: HashMap<String, Vec<String>> = HashMap::new();
        for attr in attributes {
            if let Some(current) = map.get_mut(&attr.attribute) {
                current.push(attr.value)
            } else {
                map.insert(attr.attribute, vec![attr.value]);
            }
        }
        return map.into_iter().collect_vec();
    }
}

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    usize,
};

use image::Rgb;
use imageproc::{
    drawing::{draw_hollow_rect_mut, draw_text_mut},
    rect::Rect,
};
use itertools::Itertools;
use log::*;
use rusttype::{Font, Scale};
use serde::{Deserialize, Serialize};

use crate::errors::SceneError;

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
    name: String,
    /// coordinates (x, y) of top left corner of bounding box of the object, in pixels counted from
    /// top-left corner of the image
    top_left_corner: (u32, u32),
    /// size (width, height) of the bounding box in pixels
    size: (u32, u32),
    /// name of parent object, None if this object isn't part of some other object
    parent: Option<String>,
    /// names of child objects, empty vec if this object doesn't consist of other objects.
    children: Vec<String>,
    /// list of attributes (key, value) of this object
    attributes: Vec<(String, String)>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
/// represents triplet 'from --predicate--> to'
pub struct Triplet {
    /// source node
    pub from: String,
    /// predicate (connection label)
    pub predicate: String,
    /// target node
    pub to: String,
}

impl ToString for Triplet {
    fn to_string(&self) -> String {
        return format!(
            "{} {} {}",
            self.from.replace(' ', "_"),
            self.predicate.replace(' ', "_"),
            self.to.replace(' ', "_")
        );
    }
}

impl SceneObject {
    /// Returns a list of all attribute names that are in this object.
    pub fn get_attribute_names(&self) -> Vec<String> {
        return self
            .attributes
            .iter()
            .map(|(attr_name, _attr_val)| return attr_name.clone())
            .collect_vec();
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
    pub fn get_attributes(&self) -> Vec<(String, String)> {
        return self.attributes.clone();
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
        if let Some(parent_name) = &self.parent {
            triplets.push(Triplet {
                from: self.name.clone(),
                predicate: String::from("has parent object"),
                to: parent_name.clone(),
            })
        }
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

    /// Returns all unique attributes (key, value) present in all objects of the scene.
    pub fn get_attributes(&self) -> Vec<(String, String)> {
        return self
            .objects
            .iter()
            .flat_map(|obj| return obj.get_attributes())
            .unique()
            .collect_vec();
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

    /// Returns a list of all unique attribute names in all objects in the scene,
    /// sorted alphabetically in ascending order.
    pub fn get_attribute_names(&self) -> Vec<String> {
        return self
            .objects
            .iter()
            .flat_map(|obj| return obj.get_attribute_names())
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
            return Err(format!("can't read file '{}'", path.to_string_lossy()));
        };
        trace!("file '{}' succesfully loaded.", path.to_string_lossy());

        let scene: Scene = match serde_json::from_str(&raw_str) {
            Ok(scene) => scene,
            Err(e) => {
                return Err(format!(
                    "can't parse file '{}': {}",
                    path.to_string_lossy(),
                    e
                ));
            }
        };
        trace!(
            "file '{}' succesfully parsed into Scene",
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
            // object has reference to some parent:
            if let Some(parent_name) = &obj.parent {
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
            }

            // for all children:
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
                // 3. check if the children have this object listed as parent
                self.objects
                    .iter()
                    .filter(|o| return &o.name == child_name)
                    .for_each(|child| match &child.parent {
                        None => errs.push(SceneError::MissingParent {
                            child: child.name.clone(),
                            parent: obj.name.clone(),
                        }),
                        Some(parent_name) => {
                            if parent_name != &obj.name {
                                errs.push(SceneError::ParentMismatch {
                                    child: child.name.clone(),
                                    current: parent_name.clone(),
                                    target: obj.name.clone(),
                                })
                            }
                        }
                    })
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
            if let Some(current) = map.get_mut(&attr.0) {
                current.push(attr.1)
            } else {
                map.insert(attr.0, vec![attr.1]);
            }
        }
        return map.into_iter().map(|(k, v)| return (k, v)).collect_vec();
    }
}

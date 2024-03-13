use log::debug;

use crate::dataloader::{SceneObject, Triplet};

/// Errors related to [Scene]
pub enum SceneError {
    /// some objects in the same scene have the same name
    DuplicitName {
        /// the name that occurs in multiple objects
        name: String,
        /// number of detected occurrences under that name
        number_of_occurences: u32,
    },
    /// bounding box of object is defined in such a way that is would reach out of the underlying image.
    BboxOutOfBounds {
        /// object that has the overflowing bounding box
        object: SceneObject,
        /// size of the underlying image in pixels
        scene_size: (u32, u32),
    },
    /// object is referring to parent object which isn't specified in the scene
    ParentNotFound {
        /// name of the child object
        child: String,
        /// name of the (missing) referenced parent object
        parent: String,
    },
    /// object is referring to child object which isn't specified in the scene
    ChildNotFound {
        /// name of the (missing) referenced child object
        child: String,
        /// name of the parent object
        parent: String,
    },
    /// object A is referenced as a parent of another object B, but A doesn't specify B as it's child
    MissingChild {
        /// name of the child object
        child: String,
        /// name of the parent
        parent: String,
    },
    /// object A is referenced as a child of another object B, but A doesn't specify B as it's parent
    MissingParent {
        /// name of the child object
        child: String,
        /// name of the parent object
        parent: String,
    },
    /// object A is referenced as a child by another object B, but A specify different object C as it's parent
    ParentMismatch {
        /// name of the child object
        child: String,
        /// name of the currently referenced parent
        current: String,
        /// name of the object that references this child object as it's child
        target: String,
    },
    /// object references itself as it's own child or parent
    SelfReference {
        /// name of the object
        name: String,
    },
    /// scene references some image that can't be found
    ImageNotFound {
        /// specified path to the image file
        path: String,
    },
    /// triplet specifies source object ('from' field) that isn't defined in the scene
    TripletFromNotFound {
        /// triplet that has the invalid 'from' reference
        triplet: Triplet,
    },
    /// triplet specifies target object ('to' field) that isn't defined in the scene
    TripletToNotFound {
        /// triplet that has the invalid 'to' reference
        triplet: Triplet,
    },
}

impl SceneError {
    /// generates long human-readable string that describes the issue
    pub fn long_info(&self) -> String {
        return match &self {
            SceneError::DuplicitName {
                name,
                number_of_occurences,
            } => format!(
                "duplicit name '{}' found for {} objects",
                name, number_of_occurences
            ),
            SceneError::BboxOutOfBounds { object, scene_size } => format!(
                "bounding box of object '{}' reaches out of image",
                object.get_name(),
            ),
            SceneError::ParentNotFound {
                child: child_name,
                parent: parent_name,
            } => format!(
                "child object '{}' references parent object '{}' which does not exist",
                child_name, parent_name
            ),
            SceneError::ChildNotFound {
                child: child_name,
                parent: parent_name,
            } => format!(
                "parent object '{}' references child object '{}' which does not exist",
                parent_name, child_name
            ),
            SceneError::MissingParent {
                child: child_name,
                parent: parent_name,
            } => format!("parent object '{}' references child object '{}', but the child doesn't reference the parent back",
                parent_name, child_name
            ),
            SceneError::MissingChild {
                child: child_name,
                parent: parent_name,
            } => format!("child object '{}' references parent object '{}', but the parent doesn't reference the child back",
                child_name, parent_name
            ),
            SceneError::ParentMismatch {
                child: child_name,
                current: current_parent_name,
                target: target_parent_name,
            } => format!("parent object '{}' references child object '{}', but the child references another parent object '{}'", 
                target_parent_name, child_name, current_parent_name
            ),
            SceneError::SelfReference { name } => format!("object '{}' references itself (as a child or parent)", name),
            SceneError::ImageNotFound { path } => format!("scene referes to image file '{}' which is not found or is not readable", path),
            SceneError::TripletFromNotFound { triplet } => format!("object '{}' in triplet '{}' does not exist", triplet.from, triplet), 
            SceneError::TripletToNotFound { triplet } => format!("object '{}' in triplet '{}' does not exist", triplet.to, triplet),
        };
    }
}

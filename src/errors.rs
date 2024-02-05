use crate::dataloader::SceneObject;

pub enum SceneError {
    DuplicitName {
        name: String,
        number_of_occurences: u32,
    },
    BboxOutOfBounds {
        object: SceneObject,
        scene_size: (u32, u32),
    },
    ParentNotFound {
        child_name: String,
        parent_name: String,
    },
    ChildNotFound {
        child_name: String,
        parent_name: String,
    },
    MissingChild {
        child_name: String,
        parent_name: String,
    },
    MissingParent {
        child_name: String,
        parent_name: String,
    },
    ParentMismatch {
        child_name: String,
        current_parent_name: String,
        target_parent_name: String,
    },
    SelfReference {
        name: String,
    },
    ImageNotFound {
        path: String,
    },
}

impl SceneError {
    pub fn long_info(&self) -> String {
        return match &self {
            SceneError::DuplicitName {
                name,
                number_of_occurences,
            } => format!(
                "Duplicit name '{}' found for {} objects.",
                name, number_of_occurences
            ),
            SceneError::BboxOutOfBounds { object, scene_size } => format!(
                "Bounding box of object '{}' reaches out of image by [x: {}, y: {}] pixels.",
                object.get_name(),
                scene_size.0 - (object.get_bbox_top_left_corner().0 + object.get_bbox_size().0),
                scene_size.1 - (object.get_bbox_top_left_corner().1 + object.get_bbox_size().1),
            ),
            SceneError::ParentNotFound {
                child_name,
                parent_name,
            } => format!(
                "Child object '{}' references parent object '{}' which does not exist.",
                child_name, parent_name
            ),
            SceneError::ChildNotFound {
                child_name,
                parent_name,
            } => format!(
                "Parent object '{}' references child object '{}' which does not exist.",
                parent_name, child_name
            ),
            SceneError::MissingParent {
                child_name,
                parent_name,
            } => format!("Parent object '{}' references child object '{}', but the child doesn't reference the parent back.", 
                parent_name, child_name
            ),
            SceneError::MissingChild {
                child_name,
                parent_name,
            } => format!("Child object '{}' references parent object '{}', but the parent doesn't reference the child back.", 
                child_name, parent_name
            ),
            SceneError::ParentMismatch {
                child_name,
                current_parent_name,
                target_parent_name,
            } => format!("Parent object '{}' references child object '{}', but the child references another parent object '{}'", 
                target_parent_name, child_name, current_parent_name
            ),
            SceneError::SelfReference { name } => format!("Object '{}' references itself (as a child or parent)", name),
            SceneError::ImageNotFound { path } => format!("Provided image file '{}' is not found or is not readable.", path)
        };
    }
}

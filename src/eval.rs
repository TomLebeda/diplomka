use std::collections::HashMap;

use itertools::Itertools;
use log::{info, warn};
use rayon::iter::{FromParallelIterator, IntoParallelRefIterator, ParallelIterator};

use crate::{
    dataloader::{Attribute, Extract, ExtractData, LossTable, Scene, Score, Triplet},
    utils::remove_number_from_obj,
};

/// Filter provided extracts and keep only the objects (lifted)
fn filter_objects(extracts: &[Extract]) -> Vec<&str> {
    return extracts
        .iter()
        .filter_map(|ext| {
            return match &ext.data {
                ExtractData::Object(obj) => Some(obj.as_str()),
                _ => None,
            };
        })
        .collect_vec();
}

/// Filter provided extracts and keep only the attributes (lifted)
fn filter_attributes(extracts: &[Extract]) -> Vec<&Attribute> {
    return extracts
        .iter()
        .filter_map(|ext| {
            return match &ext.data {
                ExtractData::Attribute(attr) => Some(attr),
                _ => None,
            };
        })
        .collect_vec();
}

/// Filter provided extracts and keep only the triplets (lifted)
fn filter_triplets(extracts: &[Extract]) -> Vec<&Triplet> {
    return extracts
        .iter()
        .filter_map(|ext| {
            return match &ext.data {
                ExtractData::Triplet(trip) => Some(trip),
                _ => None,
            };
        })
        .collect_vec();
}

/// helper function for initializing the 'grouped_missing_objects' hashmap of [Score]
fn init_grouped_missing_objects(scene: &Scene) -> HashMap<String, f32> {
    return HashMap::from_par_iter(
        scene
            .get_tags()
            .par_iter()
            .map(|tag| return (tag.to_string(), 0.0)),
    );
}

/// helper function for initializing the 'grouped_missing_attributes' hashmap of [Score]
fn init_grouped_missing_attributes(scene: &Scene) -> HashMap<String, f32> {
    return HashMap::from_par_iter(
        scene
            .get_attributes()
            .par_iter()
            .map(|a| return (a.attribute.to_string(), 0.0)),
    );
}

/// helper function for initializing the 'grouped_missing_attributes' hashmap of [Score]
fn init_grouped_missing_triplets(scene: &Scene) -> HashMap<String, f32> {
    return HashMap::from_par_iter(
        scene
            .get_all_triplets_with_hierarchy()
            .par_iter()
            .map(|t| return (t.predicate.to_string(), 0.0)),
    );
}
/// helper function for initializing the 'grouped_missing_attributes' hashmap of [Score]
fn init_grouped_wrong_values(scene: &Scene) -> HashMap<String, f32> {
    return HashMap::from_par_iter(
        scene
            .get_attributes()
            .par_iter()
            .map(|a| return (a.attribute.to_string(), 0.0)),
    );
}

/// Runs evaluation on the provided scene and list of extracts.
/// Prints out some statistics and comparisons that could be used as a feature vectors.
pub fn eval(scene: Scene, extracts: Vec<Extract>, loss_table: LossTable, verbose: bool) -> Score {
    let mut grouped_missing_objects = init_grouped_missing_objects(&scene);
    let mut grouped_missing_attributes = init_grouped_missing_attributes(&scene);
    let mut grouped_missing_triplets = init_grouped_missing_triplets(&scene);
    let mut grouped_wrong_values = init_grouped_wrong_values(&scene);

    let extracted_objects = filter_objects(&extracts);
    let extracted_attributes = filter_attributes(&extracts);
    let extracted_triplets = filter_triplets(&extracts);

    // handle missing objects
    let mut missing_objects = 0.0;
    for obj in scene.get_all_objects() {
        let name = &obj.name.as_str();
        if extracted_objects.contains(name) {
            continue;
        };
        let loss = match name.contains('#') {
            // we didn't found it immediately, so check if ignoring numbering could help
            false => {
                if verbose {
                    warn!("missing object: {}", name)
                }
                loss_table.get_loss_missing_obj(obj)
            }
            true => {
                let numberless_name = remove_number_from_obj(name);
                match extracted_objects.contains(&numberless_name) {
                    true => {
                        if verbose {
                            warn!("numberless object: {}", name)
                        }
                        loss_table.numberless_penalty
                    }
                    false => {
                        if verbose {
                            warn!("missing object: {}", name)
                        }
                        loss_table.get_loss_missing_obj(obj)
                    }
                }
            }
        };
        missing_objects += loss;
        for tag in &obj.tags {
            if let Some(val) = grouped_missing_objects.get_mut(tag) {
                *val += loss;
            }
        }
    }

    // handle missing attributes
    let mut missing_attributes = 0.0;
    for attr in scene.get_attributes() {
        if extracted_attributes.contains(&&attr) {
            continue;
        }
        let loss = match attr.object.contains('#') {
            // we didn't found it immediately, so check if ignoring numbering could help
            false => {
                if verbose {
                    warn!("missing attribute: {}", attr)
                }
                loss_table.get_loss_missing_attr(&attr.attribute)
            }
            true => {
                // the object is numbered, so try to ignore it
                let numberless_name = remove_number_from_obj(&attr.object);
                match extracted_objects.contains(&numberless_name) {
                    true => loss_table.numberless_penalty,
                    false => {
                        if verbose {
                            warn!("missing attribute: {}", attr)
                        }
                        loss_table.get_loss_missing_attr(&attr.attribute)
                    }
                }
            }
        };
        missing_attributes += loss;
        if let Some(v) = grouped_missing_attributes.get_mut(&attr.attribute) {
            *v += loss;
        }
    }

    // handle missing triplets
    let mut missing_triplets = 0.0;
    for trip in scene.get_all_triplets_with_hierarchy() {
        if extracted_triplets.contains(&&trip) {
            continue;
        }
        let ignore_num_from = Triplet {
            from: remove_number_from_obj(&trip.from).to_string(),
            predicate: trip.predicate.clone(),
            to: trip.to.clone(),
        };
        let ignore_num_to = Triplet {
            from: trip.from.clone(),
            predicate: trip.predicate.clone(),
            to: remove_number_from_obj(&trip.to).to_string(),
        };
        let ignore_num_both = Triplet {
            from: remove_number_from_obj(&trip.from).to_string(),
            predicate: trip.predicate.clone(),
            to: remove_number_from_obj(&trip.to).to_string(),
        };
        let loss = if extracted_triplets.contains(&&ignore_num_from)
            || extracted_triplets.contains(&&ignore_num_to)
        {
            loss_table.numberless_penalty
        } else if extracted_triplets.contains(&&ignore_num_both) {
            loss_table.numberless_penalty * 2.0
        } else {
            if verbose {
                warn!("missing triplet: {}", trip)
            }
            loss_table.get_loss_missing_triplet(&trip.predicate)
        };
        missing_triplets += loss;
        if let Some(v) = grouped_missing_triplets.get_mut(&trip.predicate) {
            *v += loss;
        }
    }

    // handle wrong values
    let mut wrong_values = 0.0;
    for a in extracted_attributes {
        if let Some(obj) = scene.get_object(&a.object) {
            // found the object
            let ref_values = obj
                .attributes
                .iter()
                .filter(|at| return at.0 == a.attribute) // keep attributes that match by name
                .map(|at| return &at.1) // extract the values
                .collect_vec();
            if ref_values.contains(&&a.value) {
                // there is exact match => no loss
                continue;
            } else {
                // there is no exact value match => compute loss
                // if there is multiple possible target values (two colors for example)
                // then we will use the minimum of those values
                let loss = ref_values
                    .iter()
                    .map(|rv| -> f32 {
                        return loss_table.get_loss_wrong_value(&a.value, rv, &a.attribute);
                    })
                    .reduce(f32::min) // f32 doesn't support min() because of NaN
                    .unwrap_or(loss_table.wrong_values);
                wrong_values += loss;
                if let Some(v) = grouped_wrong_values.get_mut(&a.attribute) {
                    *v += loss;
                };
            }
        } else {
            // no object found => try ignoring numbers
            let loss = scene
                .get_all_objects()
                .iter()
                .filter(|obj| return remove_number_from_obj(&obj.name) == a.object)
                .map(|obj| -> f32 {
                    let ref_values = obj
                        .attributes
                        .iter()
                        .filter(|at| return at.0 == a.attribute) // keep attributes that match by name
                        .map(|at| return &at.1) // extract the values
                        .collect_vec();
                    if ref_values.contains(&&a.value) {
                        return 0.0;
                    } else {
                        // there is no exact match => compute loss
                        // if there is multiple possible target values (two color for example)
                        // then we will use the minimum of those loss values
                        return ref_values
                            .iter()
                            .map(|rv| -> f32 {
                                return loss_table.get_loss_wrong_value(&a.value, rv, &a.attribute);
                            })
                            .reduce(f32::min) // f32 doesn't support min() because of NaN
                            .unwrap_or(loss_table.wrong_values);
                    }
                })
                .reduce(f32::min)
                .unwrap_or(loss_table.wrong_values);
            let loss = loss + loss_table.numberless_penalty;
            if verbose {
                warn!("wrong value: {}", a)
            }
            wrong_values += loss;
            if let Some(v) = grouped_wrong_values.get_mut(&a.attribute) {
                *v += loss;
            }
        }
    }
    let score = Score {
        missing_objects,
        missing_attributes,
        missing_triplets,
        wrong_values,
        grouped_missing_objects,
        grouped_missing_attributes,
        grouped_missing_triplets,
        grouped_wrong_values,
    };
    info!("evaluation: {:#?}", &score);
    return score;
}

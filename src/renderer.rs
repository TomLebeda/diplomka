use clap::builder::OsStr;
use itertools::Itertools;
use log::*;

use crate::dataloader::Scene;

impl Scene {
    /// Constructs a string that is graph of the scene in a DOT language
    pub fn render_graph_dot_str(&self) -> String {
        let mut buf = String::new();
        buf += format!("digraph {} {{\n", &self.get_image_name()).as_str();
        buf += "node[fontname=\"Iosevka Slab\"]\n";
        buf += "edge[fontname=\"Iosevka Slab\"]\n";
        buf += format!(
            "graph[fontname=\"Iosevka Slab\",label=\"{}\",labelloc=t,layout=dot,rankdir=LR]\n",
            self.get_image_name()
        )
        .as_str();
        buf += self
            .get_all_objects()
            .iter()
            .map(|obj| return obj.dot_node_str())
            .join("\n")
            .as_str();
        buf += "\n";
        buf += self
            .get_all_triplets()
            .iter()
            .map(|triplet| {
                return format!(
                    "\"{}\" -> \"{}\" [label=\"{}\"]",
                    triplet.from, triplet.to, triplet.predicate
                );
            })
            .join("\n")
            .as_str();
        buf += "\n}"; // close the digraph
        return buf;
    }

    /// creates a visual graph representation from the scene
    pub fn render_graph(&self) {
        trace!(
            "rendering graph of {}",
            self.get_image_path().to_string_lossy()
        );
        let graph_str = &self.render_graph_dot_str();
        println!("{}", graph_str);
    }
}

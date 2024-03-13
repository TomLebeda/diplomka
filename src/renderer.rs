use std::{io::Write, path::PathBuf, process::Command};

use clap::builder::OsStr;
use image::error;
use itertools::Itertools;
use log::*;

use crate::dataloader::{Scene, SceneObject};

impl SceneObject {
    /// Returns a string that represents the object as a Node in DOT language.
    pub fn dot_node_str(&self) -> String {
        let key_color = "#7638c5";
        let value_color = "#393552";
        let obj_name = self.get_name();
        let top_row = format!(
            "<tr>
                <td bgcolor=\"black\" colspan=\"2\" align=\"LEFT\"><B><FONT color=\"white\">{}</FONT></B></td>
            </tr>", obj_name
        );
        let mut other_rows = self
            .get_attributes()
            .iter()
            .sorted_unstable_by_key(|(attr, val)| return attr)
            .map(|(attr, val)| {
                return format!(
                    "<tr>
                        <td align=\"LEFT\"><FONT color=\"{}\">{}</FONT></td>
                        <td align=\"LEFT\"><FONT color=\"{}\">{}</FONT></td>
                    </tr>",
                    key_color, attr, value_color, val
                );
            })
            .collect_vec();
        let center_str = format!("[{}, {}]", self.get_center().0, self.get_center().1);
        other_rows.push(format!(
            "<tr>
                <td align=\"LEFT\"><FONT color=\"{}\">bbox center</FONT></td>
                <td align=\"LEFT\"><FONT color=\"{}\">{}</FONT></td>
            </tr>",
            key_color, value_color, center_str
        ));
        other_rows.push(format!(
            "<tr>
                <td align=\"LEFT\"><FONT color=\"{}\">bbox width</FONT></td>
                <td align=\"LEFT\"><FONT color=\"{}\">{}</FONT></td>
            </tr>",
            key_color,
            value_color,
            self.get_bbox_size().0
        ));
        other_rows.push(format!(
            "<tr>
                <td align=\"LEFT\"><FONT color=\"{}\">bbox height</FONT></td>
                <td align=\"LEFT\"><FONT color=\"{}\">{}</FONT></td>
            </tr>",
            key_color,
            value_color,
            self.get_bbox_size().1
        ));
        let table_content = format!("{}{}", top_row, other_rows.join("\n"));
        let label = format!("<<table cellspacing=\"0\" cellpadding=\"4\">{table_content}</table>>");
        let content = format!("shape=plain,label={label}");
        return format!("\"{obj_name}\"[{content}]");
    }
}

impl Scene {
    /// Constructs a string that is graph of the scene in a DOT language
    pub fn graph_dot_str(&self) -> String {
        trace!("constructing DOT graph string from scene...");
        let mut buf = String::new();
        buf += format!("digraph {} {{\n", &self.get_image_name()).as_str();
        buf += "node[fontname=\"Iosevka Slab\"]\n";
        buf += "edge[fontname=\"Iosevka Slab\",arrowhead=open]\n";
        buf += format!(
            "graph[fontname=\"Iosevka Slab\",label=\"scene from image: {}\",rankdir=LR,labelloc=t]\n",
            self.get_image_name()
        )
        .as_str();
        // add objects
        trace!("adding objects into the graph...");
        buf += self
            .get_all_objects()
            .iter()
            .map(|obj| return obj.dot_node_str())
            .join("\n")
            .as_str();
        buf += "\n";
        // add triplets
        trace!("adding triplets into the graph...");
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
        // add hierarchy links
        trace!("adding hierarchy links into the graph...");
        self.get_all_objects().iter().for_each(|obj| {
            for child in obj.get_children() {
                buf += format!(
                    "\"{}\" -> \"{}\" [label=\"contains\"]\n",
                    obj.get_name(),
                    child
                )
                .as_str();
            }
            for parent in obj.get_parents() {
                buf += format!(
                    "\"{}\" -> \"{}\" [label=\"is part of\"]\n",
                    obj.get_name(),
                    parent
                )
                .as_str();
            }
        });
        buf += "}"; // close the digraph
        trace!("finishing DOT graph construction");
        return buf;
    }

    /// creates a visual graph representation from the scene using DOT engine
    pub fn render_dot_graph(&self, out_file_name: &PathBuf, unflatten: bool, dump: bool) {
        trace!(
            "rendering graph of {} using DOT engine",
            self.get_image_path().to_string_lossy()
        );
        let mut graph_str = self.graph_dot_str().into_bytes();
        trace!("obtained {} bytes of DOT graph string", graph_str.len());

        if unflatten {
            // unflatten the graph for better aspect ratio
            trace!("unflatteninig the graph...");
            let Ok(mut unflatten_process) = Command::new("unflatten")
                .args(["-c", "10"])
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::piped())
                .spawn()
            else {
                error!("failed to execute 'unflatten' command.");
                std::process::exit(1);
            };

            // write the input string into the unflatten process
            if let Some(mut stdin) = unflatten_process.stdin.take() {
                let write_res = stdin.write_all(&graph_str);
                if write_res.is_err() {
                    error!("failed to pipe DOT graph into 'unflatten' command");
                } else {
                    trace!("piped {} bytes into 'unflatten' command", graph_str.len());
                }
            } else {
                error!("failed to take the stdin of 'unflatten' process");
                std::process::exit(1);
            }

            // capture the output of the unflatten_process
            let Ok(unflattened) = unflatten_process.wait_with_output() else {
                error!("failed to read 'unflatten' output");
                std::process::exit(1);
            };

            if !unflattened.status.success() {
                error!("'unflatten' failed with error: {:?}", unflattened.status);
                std::process::exit(1);
            }
            graph_str = unflattened.stdout;
        }
        if dump {
            println!("{}", String::from_utf8_lossy(&graph_str));
            std::process::exit(0);
        }

        // execute the dot command with the DOT input string
        let Ok(mut dot_process) = Command::new("dot")
            .args(["-Tsvg"])
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()
        else {
            error!("failed to execute dot command.");
            std::process::exit(1);
        };

        // write DOT input string to stdin of the dot process
        if let Some(mut stdin) = dot_process.stdin.take() {
            let write_res = stdin.write_all(&graph_str);
            if write_res.is_err() {
                error!("failed to pipe unflattened DOT graph into 'dot' command");
            }
        } else {
            error!("failed to take the stdin of 'dot' process");
            std::process::exit(1);
        }

        // capture the output of the dot process
        let Ok(output) = dot_process.wait_with_output() else {
            error!("failed to read 'dot' output");
            std::process::exit(1);
        };

        if !output.status.success() {
            error!("'dot' failed with error: {:?}", output.status);
            std::process::exit(1);
        }

        std::fs::write(out_file_name, output.stdout);
        info!("oputput written into {}", out_file_name.to_string_lossy());
    }
}

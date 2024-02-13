# #!/bin/nu
# def main [crumb_str: string] {
# 	let tags = $crumb_str | lines;
# 	let parsed = $tags | parse "{obj} {pred} {sub}";
# 	let objects = $parsed.obj | str replace -r "_#\\d+" "" | uniq
# 	let subjects = $parsed.sub | str replace -r "_#\\d+" "" | uniq
# 	print $parsed
# }

let mapping = {
	has_color: "$OBJ_has_color_SUBJ = ( $OBJ (je | má barvu) $SUBJ | $OBJ má $SUBJ barvu | $SUBJ $OBJ) {OBJ has_color SUBJ};"
	has_child_object: "$OBJ_has_child_object_SUBJ = OBJ má SUBJ {OBJ has_child_object SUBJ};",
	is_part_of: "$OBJ_is_part_of_SUBJ = OBJ (je [sou]částí | patří [pod|k] | tvoří | náleží) SUBJ {OBJ is_part_of SUBJ};",
	chasing: "$OBJ_chasing_SUBJ = OBJ (honí | prosnásleduje | běží za | prohání) SUBJ {OBJ chasing SUBJ};",
	running_away_from: "$OBJ_running_away_from_SUBJ = OBJ (utíká | běží | zdrhá) (od | před) SUBJ {OBJ running_away_from SUBJ};",
	sitting_on: "$OBJ_sitting_on_SUBJ = OBJ sedí na SUBJ {OBJ sitting_on SUBJ};",
	is_hidden_behind: "$OBJ_is_hidden_behind_SUBJ = OBJ [je] [schovan(ý|á|é) | skryt(ý|á|é)] (za | zpoza) SUBJ {OBJ is_hidden_behind SUBJ};",
	climbing: "$OBJ_climbing_SUBJ = OBJ (šplhá | leze) [na] SUBJ {OBJ climbing SUBJ};"
}
let parsed = ./target/release/diplomka crumble ./data/summer_simple.json | rg -v -e "[x|y]_[max|min]" | lines | str replace -r "_#\\d+" "" | uniq | parse "{obj} {pred} {subj}"
$parsed | each {|line| $mapping | get $line.pred | str replace "OBJ" $line.obj -a | str replace "SUBJ" $line.subj -a} | to text

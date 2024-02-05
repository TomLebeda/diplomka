#!/bin/nu
def main [f: string] {
	let file = open $f;
	let triplets = $file | each {|row| $"($row.subject) -> ($row.object): ($row.predicate)"};
	let outfile = ($f | str replace ".csv" ".d2");
	$triplets | save -f $outfile
}

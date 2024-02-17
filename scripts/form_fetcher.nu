#!/bin/nu
def main [word: string] {
	try {
		let sections = http get $"https://cs.wiktionary.org/w/api.php?action=parse&format=json&page=($word)" | get parse.sections;
		1 / 0
	} catch { |e| print -e $"fetching failed: ($e.msg)" }
}

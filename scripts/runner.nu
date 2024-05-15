let grammar = "./data/grammars/summer.spgf"
let scene = "./data/scenes/summer.json"
let loss = "./data/loss.json"
alias run = ./target/release/diplomka

run extract $scene $grammar data/out/gpt1/input.txt data/out/gpt1/extracts.json
run evaluate data/out/gpt1/extracts.json $scene $loss | save -f ./data/out/gpt1/score.txt

run extract $scene $grammar data/out/gpt2/input.txt data/out/gpt2/extracts.json
run evaluate data/out/gpt2/extracts.json $scene $loss | save -f ./data/out/gpt2/score.txt

run extract $scene $grammar data/out/gpt3/input.txt data/out/gpt3/extracts.json
run evaluate data/out/gpt3/extracts.json $scene $loss | save -f ./data/out/gpt3/score.txt

# run extract $scene $grammar data/out/nos4/input.txt data/out/nos4/extracts.json
# run evaluate data/out/nos4/extracts.json $scene $loss | save -f ./data/out/nos4/score.txt
#
# run extract $scene $grammar data/out/nos5/input.txt data/out/nos5/extracts.json
# run evaluate data/out/nos5/extracts.json $scene $loss | save -f ./data/out/nos5/score.txt

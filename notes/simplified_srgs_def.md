# SIMPLIFIED SRGS
(Original full SRGS specification)[https://www.w3.org/TR/speech-grammar]

## FORMAL SYNTAX DEFINITION:
Grammar := [Header] Rule*

Header := `#ABNF 1.0 UTF-8;`

Rule := [`public`] RuleName `=` RuleBody `;`

RuleName := `$`

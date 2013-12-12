REBOL[
	Title: "R2E2 - REBOL Regular Expressions Engine"
	Date: 12-12-2013
	File: %regex.r
	Author: "Boleslav Brezovsky"
	Version: 0.1.3
	History: [
		0.1.3	12-12-2013 [
			"Uploaded to GITHUB"
			"Changel license to MIT"
		]
		0.1.2	28-5-2007 [
			"'tail-parse - endless loop fixed (hopefully)"
		]
		0.1.1	27-5-2007 [
			"'regex - matches string against regex rules, that are translated by VV"
			"'translate-regex - translates rexes to set of parse rules for tail-parse"
			"enclosed in context 'ctx-regex"
			"'regset - new and better version from brianh"
			"'bitset-to-string - faster, brianh"
		]
		0.1.0	26-5-2007 ""
	]
	License: 'MIT
	ToDo: [
		{| - missing}
	]
	Known-issues: [
		{some rules can lead to endless loops [abcd]*...}
	]
]


bitset-to-string: func [b [bitset!] /local s x] [
    s: copy either find b 0 ["^(00)"] [""]
    repeat x 255 [
        if find b x [append s to-char x]
    ]
    s
]

;***current tail-parse problem
;to prevent [some [any [rule]]]
;some is not added in case of any
;this is not a solution, any cannot be parsed at all in case:
;"abd" - [any ["b" | "d"]]
;so I really should implement some backtracing mechanism

tail-parse: func [string rules /local parsed?][
	string: copy string						;I don't want to alter string
	reverse rules: copy/deep rules	;and I also don't want to reverse rules
	until [
		;check rule and get its position
		;if rule does not match, skip to next character
		parsed?: false
		;if rule contains ANY, let me know
		equal? rules/1/1 'any
		all [
			not none? rules/1
			either equal? rules/1/1 'any [
				parse/all string [() mark: [rules/1 (parsed?: true print "tru" ) end | 1 skip mark: ()] end ()]
			][
				; [some [any rule]] will run into endless loop, so this is fix
				parse/all string [() mark: some [rules/1 (parsed?: true) end | 1 skip mark: () ] end ()]
			]	
		;if pattern is matched,
			;parsed?
		;remove matched string from the end
			remove/part mark length? mark
		]
		rules: next rules
		any [tail? rules not parsed?]
	]
	empty? string
]
 
;BRIANH's faster and better version :)
regset: func [expression /local out negate? b e x] [
    negate?: false
    out: make bitset! []
    parse/all expression [
        opt ["~" (negate?: true)]
        some [
            "-" (insert out #"-") |
            b: skip "-" e: skip (
                b: first b  e: first e
                either b > e [
                    insert out e
                    repeat x b - e [insert out e + x]
                ] [
                    insert out b
                    repeat x e - b [insert out b + x]
                ]
            ) |
            x: skip (insert out first x)
        ]
    ]
    if negate? [out: complement out]
    out
]

ctx-regex: context [

	make-subrule: func [pairs /local out][
		out: copy []
		foreach [i o] pairs [
			repend out [i to paren! reduce ['create-rule o] '| ]
		]
		head remove back tail out
	]

	digit-: charset [#"0" - #"9"]
	word-: regset "0-9a-zA-Z_"
	whitespace-: charset [#" " #"^-" #"^/"]

	hexa-: regset "0-9a-fA-F"
	newline-: charset [#"^/"]
	dot-: complement newline-

	char-groups-: make-subrule [
		"\d" 'digit-
		"\w" 'word-
		"\s" 'whitespace-
	]
	escaped-chars-: make-subrule [
		"\*" #"*"
		"\+" #"+"
		"\." #"."
		"\?" #"?"
		"\[" #"["
		"\]" #"]"
		"\(" #"("
		"\)" #")"
		"\/" #"/"
]
	whitespaces-: make-subrule [
		"\t" #"^-"	;TAB
		"\r" #{0D}	;CR
		"\n" #{0A}	;LF
		"\a" #{07}	;bell
		"\e" #{1B}	;escape
		"\f" #{0C}	;form feed
		"\v" #{0B}	;vertical tab
	]
	
	set 'translate-regex func [
		"Translate regex rules to parse rules for tail-parse"
		pattern
		/local rule expression values
	][
		rule: copy []
		values: copy []
		last-item: copy []
		current-rule: copy []
		modified?: false ;is last rule modified?
		append-rule: func [item][
			append/only rule append copy [] item
		]
		create-rule: func [item][

			last-item: item
			if not none? item [
				;if buffer is full, put it to main rule and empty it
				if not empty? current-rule [
					append/only rule copy/deep current-rule
					clear current-rule
				]
				;probe current-rule
				append current-rule reduce [reduce [item]]
			]
			modified?: false
		]
		modify-last-rule: func [item][
			insert head current-rule item
			modified?: true
		]
		repeat-rule: does [
			;repeat-rule is used only in case {nr,nr}
			;changing modified? is probably very dangerous in other cases
			modified?: false
			create-rule last-item		
		]
		parse/all pattern [
			any [
				#"*" (modify-last-rule 'any)
			|	#"+" (modify-last-rule 'some)	
			|	#"." (create-rule 'dot-)
			|	#"?" (modify-last-rule 'opt)
			|	#"[" copy expression to #"]" (
					create-rule reduce regset expression
				) 1 skip
			|	#"{" copy nr to "}" (
				parse/all nr [
					some digit- #"," some digit- (
						use [numbers lr][
							numbers: parse nr ","
							modify-last-rule to integer! numbers/1
							for i to integer! numbers/1 -1 + to integer! numbers/2 1 [
								repeat-rule
							]
						]
					)
				|	some digit- (modify-last-rule to integer! nr)
				]
				) 1 skip
			|	#"(" copy group to ")" (
					probe group
					append values group
					create-rule group
				) 1 skip
			;----
			|	escaped-chars-
			|	whitespaces-
			|	char-groups-
			|	"\x" mark: hexa- hexa- (create-rule load head append insert head copy/part mark 2 "#{" "}")
			|	s: (create-rule s/1) skip
			]
		]
		create-rule ""
		rule
	]
	set 'regex func [
		string
		pattern
	][
		tail-parse string translate-regex pattern
	]
]



demo-data: [
	"EMAIL" "hello@world.re" "[a-z0-9._%-]+@[a-z0-9.-]+\.[a-z]{2,4}"
	"DATE" "21/3/2006" "\d{1,2}\/\d{1,2}\/\d{4}"
]

demo: [
print "****TEST****"
foreach [type string pattern] demo-data [
	print [";" type newline ">> regex" mold string mold pattern "^/==" regex string pattern newline newline] ;"*****OK*****"]
]
]
;do test
"type >> do demo"
let strtok(s) = {
    let whitespace = [" ", "\n", "\t"]

    s 
    |> to_charlist 
    |> group_by(_, fn(ch) => any([ch == w for w in whitespace]))
    |> map(concat, _)
}

let replace_words(str, replacements) = str
    |> strtok
    |> replace(_, replacements)
    |> concat_sep(_, " ")

let parse_template(string) = {
    let words = string |> strtok

    let loop(words, template) = match words
	| [] -> {
	    (reverse(template), [])
	}
	| ["%endloop%" | remaining] -> {
	    (reverse(template), remaining)
	}
	| ["%startloop%", loop_var | words] -> {
	    let (inner_template, remaining) = loop(words, [])
	    let loop_template = (:loop, %{loop_var: loop_var, inner: inner_template})
	    loop(remaining, [loop_template | template])
	}
	| [word | _] -> {
	    let html_loop(words, acc) = match words
		| [] | ["%startloop%" | _] | ["%endloop%" | _] -> (reverse(acc), words)
		| [word | words] -> html_loop(words, [word | acc])

	    let (html, remaining) = html_loop(words, [])
	    let html = concat_sep(html, " ")
	    let html_template = (:html, html)
	    loop(remaining, [html_template | template])
	}

    loop(words, [])
}

let run_template(template, state) = {
    let state = state
	|> map_to_list
	|> map(fn((k, v)) => ("{{" + k + "}}", v), _)
	|> fold(%{}, fn(acc, (k, v)) => %{k => v | acc}, _)

    let loop = fn(template, acc) => match template
	| [] -> acc |> reverse |> concat
	| [(:loop, %{loop_var, inner}) | xs] -> {
	    let loop_template = fn(ls, acc) => match ls
		| [] -> acc |> reverse |> concat
		| [inner_state | xs] -> {
		    let inner_result = run_template(inner, inner_state)
		    loop_template(xs, [inner_result | acc])
		}

	    let loop_output = loop_template(state("{{" + loop_var + "}}"), [])
	    loop(xs, [loop_output | acc])
	}
	| [(:html, html) | xs] -> {
	    let html = html |> replace_words(_, state) |> replace_words(_, %{"%endloop%" => ""})
	    loop(xs, [html | acc])
	}

    loop(template, [])
}

let template_from_file(filename) = filename
    |> read_file
    |> parse_template
    |> fn((template, _)) => template

let template_file_string(filename, state) = filename
    |> template_from_file
    |> run_template(_, state)

let template_file(input_file, output_file, state) = input_file
    |> template_file_string(_, state)
    |> write_file(output_file, _)

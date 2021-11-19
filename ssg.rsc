let scan_template(string) = {
    let scan_keyword(chars, acc) = match chars
	| [] -> {
	    println("Unmatched %")
	    let () = 1
	}
	| ["%" | remaining] -> {
	    let kw = (:keyword, acc)
	    (kw, remaining)
	}
	| [ch | rest] -> {
	    scan_keyword(rest, [ch | acc])
	}

    let scan_substitution(chars, acc) = match chars
	| [] -> {
	    println("Unmatched {{")
	    let () = 1
	}
	| ["}", "}" | remaining] -> {
	    let subst = (:substitution, acc)
	    (subst, remaining)
	}
	| [ch | rest] -> {
	    scan_substitution(rest, [ch | acc])
	}

    let loop(chars, acc) = match (chars, acc)
	| ([], _) -> {
	    reverse(acc)
	}
	| (["%" | rest], _) -> {
	    let (kw, remaining) = scan_keyword(rest, [])
	    loop(remaining, [kw | acc])
	}
	| (["{", "{" | rest], _) -> {
	    let (subst, remaining) = scan_substitution(rest, [])
	    loop(remaining, [subst | acc])
	}
	| ([ch | remaining], [(:html, html_chars) | acc]) -> {
	    let html = (:html, [ch | html_chars])
	    loop(remaining, [html | acc])
	}
	| ([ch | remaining], acc) -> {
	    let html = (:html, [ch])
	    loop(remaining, [html | acc])
	}

    let postprocess((ty, chars)) = (ty, chars |> reverse |> concat)

    string
    |> to_charlist
    |> loop(_, [])
    |> map(postprocess, _)
}

let parse_template(str) = {
    let parse(toks, acc) = match toks
	| [] -> {
	    (reverse(acc), [])
	}
	| [(:html, _) as tok | xs] | [(:substitution, _) as tok | xs] -> {
	    parse(xs, [tok | acc])
	}
	| [(:keyword, "endloop") | xs] -> {
	    (reverse(acc), xs)
	}
	| [(:keyword, "startloop"), (:html, " "), (:substitution, loop_var) | xs] -> {
	    let (inner, remaining) = parse(xs, [])
	    let loop = (:loop, %{inner: inner, loop_var: loop_var})
	    parse(remaining, [loop | acc])
	}

    str
    |> scan_template
    |> parse(_, [])
}

let run_template(template, state) = {
    let state = state
	|> map_to_list
	|> fold(%{}, fn(acc, (k, v)) => %{k => v | acc}, _)

    let loop = fn(template, acc) => match template
	| [] -> acc |> reverse |> concat
	| [(:loop, %{loop_var, inner}) | xs] -> {
	    let loop_template = fn(ls, acc) => match ls
		| [] -> acc |> reverse |> concat
		| [inner_state | xs] when typeof(inner_state) == :dictionary -> {
		    let inner_state = merge_maps(inner_state, state)
		    let inner_result = run_template(inner, inner_state)
		    loop_template(xs, [inner_result | acc])
		}
		| [item | xs] -> {
		    let inner_state = %{"item" => item}
		    let inner_result = run_template(inner, inner_state)
		    loop_template(xs, [inner_result | acc])
		}

	    match state(loop_var)
		| () -> {
		    println("Missing loop variable " + loop_var)
		    let () = 1
		}
		| inner_template -> {
		    let loop_output = loop_template(inner_template, [])
		    loop(xs, [loop_output | acc])
		}
	}
	| [(:substitution, substitution) | xs] -> {
	    match state(substitution)
		| () -> {
		    println("Missing variable " + substitution)
		    let () = 1
		}
		| word -> loop(xs, [word | acc])
	}
	| [(:html, html) | xs] -> {
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

let gen_site(endpoints, output_dir, default_state) = {
    let endpoints = map_to_list(endpoints)

    let gen_endpoint = fn(endpoint, generator_state) => {
	let ((path, _), gen_fn) = endpoint
	let output = gen_fn(generator_state)
	write_file(output_dir + "/" + path, output)
    }

    # TODO: put paths from other endpoints here
    let generator_state =
	fold(default_state, fn(acc, ((path, route), _)) => %{(route + "_route") => path | acc}, endpoints)

    foreach(endpoints, gen_endpoint(_, generator_state))
}

#let endpoints = %{
#    "index.html" => fn(gen_state) => {
	#let items = [
	#    %{"title" => "Card 1", "value" => "Some stuff here"},
	#    %{"title" => "Another one", "value" => "Some more stuff here"}
	#]
	#let gen_state = %{"items" => items | gen_state}
	#template_file_string("../rustscript_site/templates/resume.html", state)
#    }
#}

#let default_state = %{
    #"header" => read_file("templates/header.html"),
    #"footer" => read_file("templates/footer.html")
#    "header" => "",
#    "footer" => ""
#}

#gen_site(endpoints, "out", default_state)

#"../rustscript_site/templates/resume.html"
#|> read_file
#|> parse_template
#|> inspect

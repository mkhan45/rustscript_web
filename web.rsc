let parse_urlencoded(uri) = {
    let pairs = uri 
	|> to_charlist 
	|> split(_, "&") 
	|> map(split(_, "="), _) 
	|> map(fn(pair) => map(concat, pair), _)

    let is_pair = fn(p) => if let [_, _] = p then T else F

    if all([is_pair(p) for p in pairs]) then
	(:ok, fold(%{}, fn(acc, [k, v]) => %{k => v | acc}, pairs))
    else
	:err
}

let remove_suffix(str, suffix) = {
    let chars = to_charlist(str)
    let strlen = length(chars)
    let suffix_len = length(suffix |> to_charlist)

    if strlen > suffix_len then {
	let (start, end) = split_at(strlen - suffix_len, chars)

	if concat(end) == suffix 
	    then concat(start) 
	    else str
    } else  {
	str
    }
}

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
	| [(:keyword, "end") | xs] -> {
	    (reverse(acc), xs)
	}
	| [(:keyword, "startloop"), (:html, " "), (:substitution, loop_var) | xs] -> {
	    let (inner, remaining) = parse(xs, [])
	    let loop = (:loop, %{inner: inner, loop_var: loop_var})
	    parse(remaining, [loop | acc])
	}
	| [(:keyword, "if"), (:html, " "), (:substitution, cond_var) | xs] -> {
	    let (inner, remaining) = parse(xs, [])
	    let if_stmt = (:if_stmt, %{inner: inner, cond: cond_var})
	    parse(remaining, [if_stmt | acc])
	}
	| [(:keyword, "ifnot"), (:html, " "), (:substitution, cond_var) | xs] -> {
	    let (inner, remaining) = parse(xs, [])
	    let ifnot_stmt = (:ifnot_stmt, %{inner: inner, cond: cond_var})
	    parse(remaining, [ifnot_stmt | acc])
	}
	| [(:keyword, "path"), (:html, " "), (:substitution, path_name) | xs] -> {
	    parse(xs, [(:path, path_name) | acc])
	}
	| [(:keyword, "include"), (:html, " "), (:substitution, template) | xs] -> {
	    parse(xs, [(:include, template) | acc])
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
	    let template_loop = fn(ls, acc) => match ls
		| [] -> acc |> reverse |> concat
		| [inner_state | xs] when typeof(inner_state) == :dictionary -> {
		    let inner_state = merge_maps(inner_state, state)
		    let inner_result = run_template(inner, inner_state)
		    template_loop(xs, [inner_result | acc])
		}
		| [item | xs] -> {
		    let inner_state = %{"item" => item}
		    let inner_result = run_template(inner, inner_state)
		    template_loop(xs, [inner_result | acc])
		}

	    match state(loop_var)
		| () -> {
		    println("Missing loop variable " + loop_var)
		    let () = 1
		}
		| inner_template -> {
		    let loop_output = template_loop(inner_template, [])
		    loop(xs, [loop_output | acc])
		}
	}
	| [(:if_stmt, %{cond, inner}) | xs] -> {
	    match state(cond)
		| () -> loop(xs, acc)
		| _ -> {
		    let if_output = run_template(inner, state)
		    loop(xs, [if_output | acc])
		}
	}
	| [(:ifnot_stmt, %{cond, inner}) | xs] -> {
	    match state(cond)
		| () -> {
		    let ifnot_output = run_template(inner, state)
		    loop(xs, [ifnot_output | acc])
		}
		| _ -> loop(xs, acc)
	}
	| [(:substitution, substitution) | xs] -> {
	    match state(substitution)
		| () -> {
		    println("Missing variable " + substitution)
		    let () = 1
		}
		| word -> {
		    loop(xs, [word | acc])
		}
	}
	| [(:path, path) | xs] -> {
	    let base_route = state(:base_route)
	    loop(xs, [base_route + path | acc])
	}
	| [(:include, template) | xs] -> {
	    let templated = template_file_string(template, state)
	    loop(xs, [templated | acc])
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

let parse_route(route_str) = {
    let loop(segments, acc) = match segments
	| [] ->
	    reverse(acc)
	| ["*"] ->
	    reverse([:wildcard | acc])
	| [s | segments] -> {
	    let ls = to_charlist(s)
	    let first_two = take(2, ls) |> concat
	    let last_two = drop(length(ls) - 2, ls) |> concat

	    if (first_two == "{{") && (last_two == "}}") then {
		let var_name = slice(ls, 2, length(ls) - 2) |> concat
		loop(segments, [(:var, var_name) | acc])
	    } else {
		loop(segments, [(:path, s) | acc])
	    }
	}

    let (method, uri) = match route_str
	| (method, uri) -> route_str
	| uri -> (:get, uri)

    let segments = uri |> to_charlist |> split(_, "/") |> map(concat, _) |> map(remove_suffix(_, ".html"), _)
    (method, loop(segments, []))
}

let bind_route((route_method, route), str, req_method) = {
    if (route_method != :any) && (route_method != req_method) then
	:none
    else {
	let segments = str |> to_charlist |> split(_, "/") |> map(concat, _) |> map(remove_suffix(_, ".html"), _)
	
	let loop(route, segments, state) = match (route, segments)
	    | ([(:path, p) | route], [s | segments]) when s == p ->
		loop(route, segments, state)
	    | ([(:var, v) | route], [s | segments]) ->
		loop(route, segments, %{v => s | state})
	    | ([], []) ->
		(:some, state)
	    | ([:wildcard], _) ->
		(:some, state)
	    | _ ->
		:none
	
	loop(route, segments, %{})
    }
}

let gen_site(base_route, endpoints, pages, output_dir, default_state) = {
    let endpoints = endpoints 
	|> map_to_list 
	|> map(fn((route, gen_fn)) => (parse_route((:get, route)), gen_fn), _)

    let gen_page = fn(page, generator_state) => {
	let loop = fn(endpoints) => match endpoints
	    | [((_, route), gen_fn) | endpoints] -> {
		match bind_route(route, page, :get)
		    | (:some, bindings) -> {
			let state = merge_maps(bindings, generator_state)
			let output = gen_fn(state)
			write_file(output_dir + "/" + page, output)
		    }
		    | :none -> {
			loop(endpoints)
		    }
	    }
	    | [] -> {
		println("No endpoints matched " + page)
		let () = 1
	    }
	loop(endpoints)
    }

    let generator_state = %{base_route: base_route | default_state}

    foreach(pages, gen_page(_, generator_state))
}

let is_path(seg) = match seg
    | (:path, _) -> T
    | _ -> F

let route_specificity((method, route)) = match route
    | [] -> 1
    | _ -> route |> filter(is_path, _) |> length

let serve_endpoints(mode, port, default_state, server_state, endpoints) = {
    let endpoints = endpoints 
	|> map_to_list 
	|> map(fn((route, gen_fn)) => (parse_route(route), gen_fn), _)
	|> sort(_, fn((a, _), (b, _)) => route_specificity(b) - route_specificity(a))

    let serve_callback = fn(uri, req_method, headers, body, server_state) => {
	let req_method = match req_method
	    | "GET" -> :get
	    | "POST" -> :post

	let uri = uri 
	    |> to_charlist 
	    |> split(_, "/")
	    |> drop(2, _) 
	    |> map(concat, _)
	    |> concat_sep(_, "/")
	    |> replace_substr(_, "%20", " ")

	let loop = fn(endpoints) => match endpoints
	    | [(route, gen_fn) | endpoints] -> {
		match bind_route(route, uri, req_method)
		    | (:some, bindings) -> {
			let state = 
			    %{base_route: "/", uri: uri, method: req_method, headers: headers, body: body | bindings}
			    |> merge_maps(_, default_state)
			gen_fn(state, server_state)
		    }
		    | :none -> {
			loop(endpoints)
		    }
	    }
	    | [] -> {
		println("No endpoints matched " + uri)
		let () = 1
	    }

	loop(endpoints)
    }

    match mode
	| (:ssl, cert_path, key_path) -> start_server_ssl(cert_path, key_path, port, serve_callback, server_state)
	| :tls -> start_server(port, serve_callback, server_state)
}

# Static Site Generation

Static site generation uses the `gen_site()` function:
`gen_site(base_route, endpoints, pages, output_dir, default_state)`

### Arguments:

`base_route`: The base url used by the %path% template directive. Prepended to all template paths
`endpoints`: An endpoint map, described later
`pages`: A list of string urls to be generated using the endpoints
`output_dir`: The folder to output the site into
`default_state`: The base state used by all page

### Example:

```ex
let endpoints = %{
    "index.html" => fn(gen_state) => {
	"Hello World"
    },
    "hello/{{name}}.html" => fn(gen_state) => {
        # template args are added to the gen state
        # template args must be a whole segment, i.e.
        # delimited completely by slashes
	"Hello " + gen_state("name")
    }
}

gen_site("", endpoints, ["index.html", "hello/qwer.html", "hello/asdf.html"], "out", %{})
```

# Running a Server

In progress

let endpoints = %{
    "index.html" => fn(gen_state) => {
	"Hello World"
    },
    "hello/{{name}}.html" => fn(gen_state) => {
	"Hello " + gen_state("name")
    }
}

gen_site("", endpoints, ["index.html", "hello/qwer.html", "hello/asdf.html"], "out", %{})

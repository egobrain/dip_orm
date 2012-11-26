REBAR= `which rebar || ./rebar`

all:
	@$(REBAR) compile skip_deps=true

deps: _deps
_deps:
	@$(REBAR) get-deps
	@$(REBAR) compile

docs:
	@$(REBAR) doc skip_deps=true

eunit:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

xref: _xref
_xref: 
	@$(REBAR) xref skip_deps=true

test: ct
ct:
	@$(REBAR) skip_deps=true ct

clean:
	@$(REBAR) clean skip_deps=true

clean_all:
	@$(REBAR) clean

build_plt:
	@$(REBAR) build_plt

dialyzer:
	@$(REBAR) analyze
serve:
	elm-live src/Main.elm -- --output=elm.js

build:
	elm make src/Main.elm --output=elm.js --optimize
.PHONY: build

deploy: build
	gh-pages -d .
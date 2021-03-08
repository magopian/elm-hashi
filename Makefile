serve:
	elm-live src/Main.elm -- --output=elm.js --optimize

deploy:
	gh-pages -d .
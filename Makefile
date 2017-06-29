live: install
	elm-live Main.elm --warn --output=app.js --open

debug: install
	elm-live Main.elm --warn --output=app.js --open --debug

build: clean-build
	mkdir build/
	cp index.html build/
	elm-make Main.elm --output build/app.js

optimize:
	uglifyjs build/app.js -c -m -o build/app.js

deploy: build optimize
	gh-pages --dist build/

clean-build:
	rm -fr build/

clean: clean-build
	rm -fr elm-stuff/ .venv build/app.js app.js

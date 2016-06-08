all: js haskell

js:
	npm install

haskell:
	stack build && stack install

.PHONY: sync admit ui install typecheck unit build e2e verify
admit:
	python3 scripts/verify-gates.py
sync: admit
	ggen sync run
ui:
	pnpm ui:sync
install:
	pnpm install

typecheck:
	pnpm typecheck
unit:
	pnpm test
build:
	pnpm build
e2e:
	pnpm test:e2e
verify: admit sync ui typecheck unit build e2e
	ggen sync run
	git diff --exit-code

.PHONY: sync verify
sync:
	ggen sync run
verify:
	pnpm verify
	ggen sync run
	git diff --exit-code

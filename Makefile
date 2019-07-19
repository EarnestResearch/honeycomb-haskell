build:
	nix-build

shell:
	nix-shell

hoogle:
	nix-shell shell-hoogle.nix --run "hoogle server --local"

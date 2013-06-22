BUILD

I like using hsenv environments to keep my package versions from conflicting:

```
./make_hsenv.sh

source ./.hsenv_hobo/bin/activate
```

If you don't care to set up an hsenv, start here:

```
cd hdiscount && cabal install

cd ../ && cabal install
```

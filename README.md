This code generates my website, a static site viewable at heytasha.com.

Building
---
Generate the `site` executable:
```
ghc --make -threaded site.hs
```

Possibilities using `site`:

* `./site clean` - Removes all traces of any previous builds
* `./site build` - Build the site
* `./site rebuild` - Clean and then build the site

To view a preview of the site (hosted locally), run `python -m
SimpleHTTPServer` from the `_site` directory. 

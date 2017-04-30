# ArchLinuxPkgStatsScraper
A project to scrape the contents of https://www.archlinux.de/?page=PackageStatistics as there is no API

http://trycatchchris.co.uk/entry/arch-linux-pkgstats-json-scraper-in-haskell

It is written using Haskell.

## Running with docker

You can use docker to execute this:
```
git clone https://github.com/chrissound/ArchLinuxPkgStatsScraper .
sudo docker build -t archscraper .
```

And then in any other directory:
```
sudo docker run -it --rm -v $(pwd):/home/archpkgstatsscraper archscraper
```

You should have a "packageStatistics.json" saved in the directory. 

## Running with stack (for development purposes)

You would need to install stack (https://haskell-lang.org/get-started)

```
stack exec ArchPkgstatsScraper
```

The output is saved to packageStatistics.json


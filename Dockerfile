FROM haskell:8.0.2

RUN cabal update && stack update
RUN apt-get update
RUN mkdir /home/archpkgstatsscraper
WORKDIR /home/archpkgstatsscraper
COPY . .
RUN stack install -v --local-bin-path /home/archpkgstatsscraper ArchPkgstatsScraper
WORKDIR /home/output
ENTRYPOINT /home/archpkgstatsscraper/ArchPkgstatsScraper --url "https://www.archlinux.de/?page=PackageStatistics"

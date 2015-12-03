# print start time
echo "DATE ||| $(date)"

# Variables
RDEVEL='R-devel_2015-12-02_r69719.tar.gz'
CRANURL='https://cran.r-project.org/src/base-prerelease/'

# Add CRAN mirror to apt-get sources
add-apt-repository "deb https://cran.rstudio.com/bin/linux/ubuntu trusty/"
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

# Swap out default Ubuntu mirror for a hopefully faster one
sed -i.bak 's/archive.ubuntu.com/mirrors.rit.edu/' /etc/apt/sources.list

# install LaTeX and some R dependencies
apt-get update && apt-get install --assume-yes --no-install-recommends \
    apache2 \
    cabal-install \
    ca-certificates \
    ccache \
    ghc \
    git \
    libcurl4-openssl-dev \
    libmysqlclient-dev \
    libpq-dev \
    libssl-dev \
    libx11-dev \
    libxml2-dev \
    mysql-client \
    subversion \
    texlive-full \
    wget

# Get dependencies to build r from source
apt-get build-dep --assume-yes r-base

# Install pandoc from source
cabal update
cabal install pandoc

# Build R from tar source
wget "$CRANURL$RDEVEL" && \
    tar -zxvf $RDEVEL && \
    cd R-devel && \
    ./configure --enable-R-shlib && \
    make && \
    make install

# Install granovaGG's dependencies
R --vanilla -f install_granovaGG_from_source.R

# Check package
R CMD build /vagrant && \
    R CMD check --as-cran /vagrant/granovaGG_1.4.0.tar.gz

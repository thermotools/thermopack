# To run this script: cd to the docs/ directory (this directory), and `bash proof_html.sh`
#
# This script runs htmlproofer (https://github.com/gjtorikian/html-proofer)
# Which is acquired when you run `bundle install`, from the 'html-proofer' gem in the Gemfile.
# We ignore files in readme_parts, because those are not meant to run on their own anyway, so in isolation they contain
# a bunch of broken internal links.
# Todo: Figure out how the cli argument for ignoring files works with regex, so that we can ignore all files in readme_parts without listing them explicitly
#
# We ignore the 403 status code, because doi.org gives us a 403, and the cli argument to ignore doi addresses does not seem to work.
#
# We need to swap /thermopack/ for /, because the server hosts the page at thermotools/thermopack, but htmlproofer reads the base of the page as /
# See: htmlproofer docs on using baseurl with jekyll.
#
# To make the test run faster (for example while you are fixing an error) you can change
# --no-disable-external => --disable-external
# to disable checking of external links.

set -e

bundle exec jekyll build # Build the site

cd _site/
bundle exec htmlproofer --no-disable-external \
--ignore-files './readme_parts/pypi_toc.html','./readme_parts/github_toc.html','./readme_parts/header.html','./readme_parts/pypi_structure.html','./readme_parts/structure.html' \
--assume-extension ./_site \
--ignore-urls 'https://fonts.gstatic.com','^https:\/\/doi\.org\/.*' \
--ignore-status-codes "403" \
--only-4xx \
--swap-urls '^/thermopack/:/' \
--no-enforce-https

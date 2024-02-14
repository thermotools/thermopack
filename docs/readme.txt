To test locally: Navigate to this directory and
    `bundle install`
    `bundle exec jekyll serve`

For info on the theme: https://github.com/pages-themes/cayman/tree/master
    The theme stylesheet is in `assets/css/style.css`
    Note: The `.page-header` property in the theme stylesheet is overwritten in `assets/css/page-header.css`
For more on GH pages: https://docs.github.com/en/pages/setting-up-a-github-pages-site-with-jekyll/testing-your-github-pages-site-locally-with-jekyll
For more on jekyl: https://jekyllrb.com/docs/pages/

Some info, conventions and a plan (if you don't understand what's going on, read the next section first):

The plan for the future is that when a new version is released:
    * The directory `vCurrent` is copied, and the copy renamed to `vX.Y.Z`.
    * The file `index.md` is copied into the directory `vX.Y.Z` and renamed to `home.md`.
     * Remember to change the `permalink` keyword at the top of the file.
    * A link to `/thermopack/vX.Y.Z/home.html` is added to all the sidebars.
    * The file `sidebar.md` is copied into `sidebar_X.Y.Z.md`, and the links are modified by changing every occurence of
        `vcurrent` to `vX.Y.Z`.
    * The `permalink` property of all the files in the `vX.Y.Z` must be changed from `/vcurrent/<my_page>` to `/vX.Y.Z/<my_page>`

Convention for permalinks:
    Permalinks ALWAYS correspond to the absolute path to the file, taking `thermopack/docs/` as the root directory.
    So the permalink to the file at `thermopack/docs/v2.1.0/home.html` is `permalink: /v2.1.0/home.html`
    NOTE: When linking to a [file](<my_link_here>) `/thermopack` must be prepended to whatever the permalink to that file
    is. So to generate a link to the file at `thermopack/docs/v2.1.0/home.html`, you would write
        [this is v2.1.0](/thermopack/v2.1.0/home.html)
    NOTE: The files you see are ´.md´ files, but they are compiled to `.html` when the page is pushed. That is why we
    link to `some_page.html`, not `some_page.md`.

Use of html-templates:
    The different templates are quite similar, but
    * Use `home.html` for "Top-level" pages, such as the homepage for each version. This modifies the title and subtitle a bit.
    * Use `default` for everything else.
    * I'm not sure if the `documentation` template is in use.

----------------------------------------------

How this works:

The "homepage" is generated from `index.md` in this directory. The name of that file cannot change (See: GitHub)

All subpages are generated from various markdown files, found in the directories
 * metapages/
  * Contains files that are common to all versions, such as citation and contact info.
 * v2.1.0/
  * Contains files that may change between versions, and apply to v2.1.0
 * vCurrent/
  * Contains files that may change between versions, and apply to the current version on GitHub.
 * memo/
  * Contains `index.md`, which links all the pdf's with memo-files, located in subdirectories under memo/

The directory `assets/` contains css for the site, and graphics. The various sidebar files are located in `_includes/`,
keep reading for more info on the sidebar.

The sidebar is dynamic, in the sense that its behaviour will change depending on what version you are viewing.
All the sidebars are generated from the markdown files found in _includes/.

The point of having a dynamic sidebar is that if someone clicks on `v2.1.0` in the sidebar, they are taken to a page
where the `Documentation`, `Installation`, `Component Identifiers` etc. links in the sidebar link to documentation and installation guides for
v2.1.0. This means we don't need to clutter the sidebar with an ever-increasing number of links to different versions, like
    Documentation v2.1.0
    Documentation v2.2.0
    ... etc ...

All markdown files used to generate subpages contain a header, with information about how they are processed by the
html-templates. They look like this:
```
---
version: 2.1.0 (Leave empty, for pages with no specific version)
sidebar_file: sidebar_2.1.0.md (Only supply this for files that should not use _layouts/sidebar.md, the default sidebar)
layout: home (The html-template to use)
title: ThermoPack v2.1.0 (The page title)
permalink: /v2.1.0/home.html (The link to use to get to this page. Note that when linking to a page, the full link `/thermopack/v2.1.0/home.html` must be used.
                                NOTE: Links cannot contain uppercase letters!)
---
```

The `version` keyword is used to configure the version number displayed at the top of the sidebar, and in the subtitle
for the page. The default value is `Latest version (beta)`, and is set in the `<h2 class="project-tagline">` element
of the html-templates.
    Note: The `description:` keyword can be used to override what appears in the subtitle (tagline) for the page.
    Note: The site defaults are supplied in `_config.yml`, e.g. in `_config.yml` : `description: A ThermoTools Project`.

The sidebar file to use is selected with a plain if-else in the html-templates. If versions other than `v2.2.0` are
added, the if-else tree must be expanded (or someone that knows html-liquid can implement automated file-path generation
based on the `version` keyword...)
    If no sidebar_file is supplied, the default `_layouts/sidebar.md` is used.





---
layout: default.liquid
title:   Hematria
published_date:    2021-12-06 18:00:00 +0000
---
## What is this?

A small program to perform gematria from the command line. Done with Haskell.

{% for post in collections.posts.pages %}
#### {{post.title}}

[{{ post.title }}]({{ post.permalink }})
{% endfor %}

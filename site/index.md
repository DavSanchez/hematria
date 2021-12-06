---
layout: default.liquid
---
# Hematria

Perform gematria from the command line. Done with Haskell.

{% for post in collections.posts.pages %}
#### {{post.title}}

[{{ post.title }}]({{ post.permalink }})
{% endfor %}

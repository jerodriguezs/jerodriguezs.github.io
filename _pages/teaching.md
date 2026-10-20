---
title: "Teaching"
permalink: /teaching/
author_profile: true
---

{% include base_path %}

<link rel="stylesheet" href="{{ base_path }}/assets/css/cards.css">

{% for group in site.data.teaching %}
## {{ group.heading }}

{% for course in group.courses %}
{% include paper-card.html paper=course %}
{% endfor %}
{% endfor %}

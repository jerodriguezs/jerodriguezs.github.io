---
layout: archive
title: "Resume"
permalink: /resume/
author_profile: true
redirect_from:
  - /resume
---

{% include base_path %}

{% for post in site.resume reversed %}
  {% include archive-single.html %}
{% endfor %}

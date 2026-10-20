---
title: "Research"
collection: research
permalink: /research/
---

{% include base_path %}

<link rel="stylesheet" href="{{ base_path }}/assets/css/cards.css">

## Publications

{% for paper in site.data.research.publications %}
{% include paper-card.html paper=paper %}
{% endfor %}

## Working Papers and Ongoing Research

{% for paper in site.data.research.working_papers %}
{% include paper-card.html paper=paper %}
{% endfor %}

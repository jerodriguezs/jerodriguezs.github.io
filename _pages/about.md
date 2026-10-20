---
permalink: /
title: "About me"
excerpt: "About me"
author_profile: true
redirect_from: 
  - /about/
  - /about.html
---

{% include base_path %}

<link rel="stylesheet" href="{{ base_path }}/assets/css/cards.css">

I'm a Postdoctoral Research Fellow at the University of Oxford. I hold a Ph.D. in Economics and Education from Columbia University. Previously, I obtained a B.A. in Economics and an M.A. in Economic Analysis from Universidad de Chile.

My research interests lie at the intersection of education and labor economics.
I'm particularly interested in implementing quantitative causal inference methods to study segregation and inequality in the educational system. Also, I'm keen on using administrative data to better understand education's impact on long-term results, such as labor outcomes and social mobility.  
Additionally, I am interested in studying algorithm systems that support decision-making processes and their consequences on equity and fairness.

## Research areas

<div class="home-areas">
  <div class="home-area">
    <h3>Education and inequality</h3>
    <p>Segregation, school resources, and teacher policy.</p>
  </div>
  <div class="home-area">
    <h3>Long-run returns</h3>
    <p>Education's effects on labor outcomes, careers, and social mobility.</p>
  </div>
  <div class="home-area">
    <h3>Algorithms and decisions</h3>
    <p>Algorithmic decision tools and their consequences for equity and fairness.</p>
  </div>
</div>

## Featured research

{% for paper in site.data.featured %}
{% include paper-card.html paper=paper %}
{% endfor %}

<p><a href="{{ base_path }}/research/">All research &rarr;</a></p>

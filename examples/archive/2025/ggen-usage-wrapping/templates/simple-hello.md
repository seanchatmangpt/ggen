---
name: simple-greeting
description: A simple hello world template for testing
version: 1.0.0
tags: [example, simple, test]
---
# Hello {{ name }}!

Welcome to **{{ project_name }}**.

## Information

- Version: {{ version }}
- Environment: {{ environment }}

{% if show_timestamp %}
Generated at: {{ timestamp }}
{% endif %}

{% if features %}
## Features

{% for feature in features %}
- {{ feature }}
{% endfor %}
{% endif %}

---

Thank you for using ggen!

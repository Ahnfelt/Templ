Templ readme
============

Templ is a language for specifying document templates.
The most common usage is to generate HTML documents on the server side.
The idea is that you specify a template and then apply the template to
different values. The following is a small template:

    <a href="mailto:$person.email">$person.name</a> | $person.name

If the `$person` has an `email` field, it links the name with a `mailto:` URL,
and otherwise it just displays the plain `name`. In other words, if given
the record `(name: {Brenda}, email: {brenda@example.com})`, it becomes

    <a href="mailto:brenda@example.com">Brenda</a>

But if given a record without an email field, such as `(name: {Peter})`, it
just becomes

    Peter
    


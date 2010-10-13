<% if(contact.getEmail() != null) { %>
  <a href="mailto:<%= contact.getEmail() %>">
  <%= contact.getName() %>
  </a>
<% } else { %>
  <%= contact.getName() %>
<% } %>
// Equivalent of Hygen's:
//   ---
//   to: app/workers/<%=name%>.js
//   ---
//   <%
//    Message = message.toUpperCase()
//   %>
//   class <%= Name %> {
//       work(){
//           return "<%= Message %>"
//       }
//   }
//
// Hygen's blessed `Name` variable (auto-capitalized `name`) is `name |
// pascal_case` here; the manual `Message = message.toUpperCase()` EJS
// scriptlet is a Tera `upper` filter applied inline instead.

pub struct Foobar;

impl Foobar {
    pub fn work(&self) -> &'static str {
        "HELLO"
    }
}

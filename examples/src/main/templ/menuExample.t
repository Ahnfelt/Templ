item :: {node:{name:String, url:String}, menu:{name:String, items:[item]}


@template showMenu(items) {
  <ul>
    items : {item|<li>showItem($item)</li>}
  </ul>
}


@template showItem(item) {
  @if($item.node) {<a href="item.node.url">item.node.name</a>}
  @else {
    div(bold(item.menu.name))
    showMenu(item.menu.items)
  }
}

@template bold(value) {<b>$value</b>}
@template div(value) {<div>$value</div>}



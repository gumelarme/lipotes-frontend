import './style.css'
import { Elm } from './Main.elm'

const app = Elm.Main.init({
  node: document.querySelector("main"),
})

app.ports.toggleDialog.subscribe(id => {
  const dialog = document.querySelector(`#${id}`)
  if(dialog.open){
    dialog.close()
  }else{
    dialog.showModal()
  }
})

app.ports.storeCollectionsBlacklist.subscribe(collections => {
  localStorage.setItem("collection_blacklist", collections);
})

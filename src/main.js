import './style.css'
import { Elm } from './Main.elm'

const STORE_KEY = "HZMSTORE";
const storedData = localStorage.getItem(STORE_KEY);
const flags = storedData ? JSON.parse(storedData) : null;

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: flags,
})

app.ports.toggleDialog.subscribe(id => {
  const dialog = document.querySelector(`#${id}`)
  if(dialog.open){
    dialog.close()
  }else{
    dialog.showModal()
  }
})

app.ports.setStore.subscribe(state => {
  localStorage.setItem(STORE_KEY, JSON.stringify(state));
})

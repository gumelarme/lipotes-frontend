/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./src/**/*.elm"
  ],
  theme: {
    extend: {},
  },
  plugins: [require("daisyui")],
}


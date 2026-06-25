document.addEventListener("input", function(event) {
  if ((event.target.tagName === "INPUT" && event.target.type === "text") || event.target.classList.contains("handsontableInput")) {
    const regex = /^[a-zA-Z0-9_-]*$/;
    if (!regex.test(event.target.value)) {
      event.target.value = event.target.value.replace(/[^a-zA-Z0-9_-]/g, "");
    }
  }
});

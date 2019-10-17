$(document).on('click', '.click_ficha', function () {
  Shiny.setInputValue("last_case", this.id, {priority: "event"});
});

$(document).on('click', '.otros_candidatos', function () {
  Shiny.setInputValue("last_cand", this.id, {priority: "event"});
});


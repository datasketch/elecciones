$(document).on('click', '.others-info', function () {
  Shiny.setInputValue("last_cand", this.id, {priority: "event"});
});


Shiny.addCustomMessageHandler("testmessage",
  function(message) {
    var modal = document.querySelector('.modal');
    modal.classList.add('is-visible')
    modal.addEventListener('click', function (event) {
      if (event.target.matches('.modal')) {
        modal.classList.remove('is-visible')
      }
    })
  }
);

Shiny.addCustomMessageHandler("otros_candidatos",
  function(id) {
    var el = document.querySelector('.input-autosuggest');
    var input = el.querySelector('input');
    input.value = id;
    el.dispatchEvent(new KeyboardEvent('keyup', {
      keyCode: 13
    }));
  }
);

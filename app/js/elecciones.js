$(document).on('click', '.click_ficha', function () {
  Shiny.setInputValue("last_case", this.id, {priority: "event"});
});

$(document).on('click', '.others-info', function () {
  Shiny.setInputValue("last_cand", this.id, {priority: "event"});
});


//var netWorkEvent = new CustomEvent("gato", {
//  detail: {
//    aaaaver: true
//  }
//});

//var modalTrigger = document.querySelector('.click_ficha');
//Lconsole.log(modalTrigger);

//var modal = document.getElementById(modalTrigger.dataset.modal);

//modalTrigger.addEventListener('click', function (event) {
//  modal.classList.add('is-visible');
//});
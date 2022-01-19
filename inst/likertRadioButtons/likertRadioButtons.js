$(document).ready(function() {
  $('.likert-input-radio.indicator-updater').change(function(){
    var text=$(this).attr('choice-name');
    var id=$(this).attr('name');
    document.getElementById(id).getElementsByClassName('likert-input-radio-indicator')[0].textContent=text;
  });
});


// create a binding object
var likertRadioButtonsBinding = new Shiny.InputBinding();

// add methods to it using jQuery's extend method
$.extend(likertRadioButtonsBinding, {

  find: function(scope) {

    // find all instances of class FrissSwitch
    return $(scope).find(".shiny-input-likert-radiobuttons");

  },
  
  getValue(el) {
    // Select the radio objects that have name equal to the grouping div's id
    const checkedItems = $(
      'input:radio[name="' + Shiny.$escape(el.id) + '"]:checked'
    );

    if (checkedItems.length === 0) {
      // If none are checked, the input will return null (it's the default on load,
      // but it wasn't emptied when calling updateRadioButtons with character(0)
      return null;
    }

    return Number(checkedItems.val());
  },
  
  setValue(el, value) {
    if ($.isArray(value) && value.length === 0) {
      // Removing all checked item if the sent data is empty
      $('input:radio[name="' + Shiny.$escape(el.id) + '"]').prop("checked", false);
    } else {
      $(
        'input:radio[name="' +
          Shiny.$escape(el.id) +
          '"][value="' +
          value +
          '"]'
      ).prop("checked", true);
    }
  },
  
  
  subscribe(el, callback) {
    $(el).on("change.likertRadioButtonsBinding", function () {
      callback(false);
    });
  },
  unsubscribe(el) {
    $(el).off(".likertRadioButtonsBinding");
  },
  
  receiveMessage: function receiveMessage(el, data) {
    
    if (data.hasOwnProperty("label")) {
      $(el)
      .find('label[for="' + 
        Shiny.$escape(el.id) + '"]')
         .text(data.label);
      }
    
    if (data.hasOwnProperty("minName")) {
      $(el)
       .find('.likert-input-radio-min').
       text(data.minName);
    }
    
    if (data.hasOwnProperty("maxName")) {
      $(el)
       .find('.likert-input-radio-max').
       text(data.maxName);
    }
    
    if (data.hasOwnProperty("choiceValues")) {
      
      var choiceInputs = $(el).find('.likert-input-radio');
      
      for (var i=0, item; item = choiceInputs[i]; i++) {
        item.setAttribute('value', data.choiceValues[i]);
        
      };
        
      var choiceLabels = $(el).find('.likert-input-radio-label');  
      
      for (var i=0, item; item = choiceLabels[i]; i++) {
        item.innerText = String(data.choiceValues[i]);
      };
    };
    
    if (data.hasOwnProperty("choiceNames")) {
      
      var choiceInputs = $(el).find('.likert-input-radio');
      
      for (var i=0, item; item = choiceInputs[i]; i++) {
        item.setAttribute('choice-name', data.choiceNames[i]);
      
      };
    };
    
    if(data.hasOwnProperty("selected")) {
      
      this.setValue(el, data.selected);
      $(el).trigger("change");
      
/*      var choiceInputs = $(el).find('.likert-input-radio');
      
      for (var i=0, item; item = choiceInputs[i]; i++) {
        if (item.value == data.selected) {
          $(item).prop("checked", true);
          
        };
      }; */
    };
  }
});
  

Shiny.inputBindings.register(likertRadioButtonsBinding);

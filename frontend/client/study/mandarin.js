instantiateChineseCard = function(card) {
  var s = card.sentences;

  for(var i=0; i<s.length; i++) {
      for(var j=0; j<s[i].blocks.length; j++) {
        var block = s[i].blocks[j];
        block.sentenceId = i;
        block.blockId    = j;
        block.len        = block.isEscaped ? 0 : block.chinese.length;
        block.literal    = false;
        block.dictIndex  = 0;
        block.isActive   = false;
      }
    }
  markNextActiveBlock(card);
  card.showAnswer = false;
}

// Clear the currently active block (if there is one) and mark the next
// available gap as active (if there is a next available gap).
function markNextActiveBlock(card) {
  var s = card.sentences;
  for(var i=0; i<s.length; i++) {
    for(var j=0; j<s[i].blocks.length; j++) {
      var block = s[i].blocks[j];
      if ( block.isActive ) {
        block.isActive = false;
        block.isGap    = false;
      } else if ( block.isGap ) {
        block.isActive = true;
        return;
      }
    }
  }
}

function activeBlock(card) {
  if (!card) return {};

  var s = card.sentences;
  for(var i=0; i<s.length; i++) {
    for(var j=0; j<s[i].blocks.length; j++) {
      var block = s[i].blocks[j];
      if ( block.isActive ) return block;
    }
  }
  return false;
}




Template.studyMandarinCard.characters = function () {
  var block = activeBlock(activeCard());
  return block.chinese;
}
Template.studyMandarinCard.card = function () { return activeCard(); }
Template.studyMandarinCard.selectedPinyin = function () {
  var card = activeCard();
  var s = card.sentences;
  var i = this.sentenceId;
  var j = this.blockId;
  var block = s[i].blocks[j];
  var dictIdx = block.dictIndex;
  return block.definitions[dictIdx].pinyin;
}
Template.studyMandarinCard.showPinyin = function () {
  return Session.get('showPinyin') && !this.isGap && !this.isEscaped;
}
Template.studyMandarinCard.isCompleted = function () {
  return activeBlock(activeCard()) == false;
}











Template.studyMandarinCard.events({
  'click .mandarin-continue': function () {
    console.log('continue');
    var card = activeCard();
    card.status = card.showAnswer ? 'missed' : 'perfect' ;
    saveCard(card);
    nextCard();
  },
  'keydown .mandarin-input': function (evt) {
    if (evt.keyCode === 27) {
      var card = activeCard();
      card.showAnswer = true;
      saveCard(card);
      console.log('show answer')
    } else if(evt.keyCode===13) {
      var card = activeCard();
      var block = activeBlock(card);
      var userAnswer = evt.currentTarget.value;
      
      var response = {stencilId: card.stencilId,
                      content: {
                        type: 'MandarinTextAnswer',
                        shownAnswer: card.showAnswer,
                        key: block.chinese,
                        value: userAnswer },
                      at: (new Date().toJSON())
                     };
      // Meteor.call('postResponse', response);

      if (userAnswer === block.chinese) {
        markNextActiveBlock(card);
        saveCard(card);
      } else {
        console.log('incorrect', userAnswer, block.chinese);
        $(evt.currentTarget).select();
      }
    }
  }
});





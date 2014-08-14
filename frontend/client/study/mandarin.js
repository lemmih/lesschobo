showMandarinAnswer = function() {
  return;
}

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
  card.status='completed';
  nextCard();
  Deps.afterFlush(function() {
    $('a.next').focus();
  });
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


Template.studyMandarinCard.headerStyle = function() {
  var card = activeCard('header');
  if ( !card ) return 'active';
  if ( card.showAnswer )
    return 'failed';
  return 'active';
};
Template.studyMandarinCard.activeBlock = function () {
  return activeBlock(activeCard('activeBlock'));
};
Template.studyMandarinCard.card = function () { return activeCard('card'); };
Template.studyMandarinCard.selectedPinyin = function () {
  var card = activeCard('selectedPinyin');
  if ( !card ) return '';
  var s = card.sentences;
  var i = this.sentenceId;
  var j = this.blockId;
  if ( !s[i] ) return '';
  var block = s[i].blocks[j];
  if ( !block || block.isEscaped ) return ''; // Why does this happen?
  var dictIdx = block.dictIndex;
  return block.definitions[dictIdx].pinyin;
};
Template.studyMandarinCard.showPinyin = function () {
  return Template.studyMandarinCard.isCompleted();
};
Template.studyMandarinCard.showAnswer = function () {
  var card = activeCard('showAnswer');
  if ( !card ) return false;
  return card.showAnswer;
};
Template.studyMandarinCard.answer = function () {
  var block = activeBlock(activeCard('answer'));
  return block.definitions[0].pinyin;
};
Template.studyMandarinCard.isCompleted = function () {
  return activeBlock(activeCard('isCompleted')) == false;
};










Template.studyMandarinCard.events({
  'click .mandarin-literal': function (evt) {
    var card       = activeCard();
    console.log('remove literal', this);
    card.sentences[this.sentenceId].blocks[this.blockId].literal = '';
    saveCard(card);
  },
  'click .mandarin-dict-select': function (evt) {
    var card       = activeCard();
    var s          = card.sentences;
    var sentenceId = $(evt.target).attr('data-sentence-id');
    var blockId    = $(evt.target).attr('data-block-id');
    var pinyinIdx  = $(evt.target).attr('data-pinyin-idx');
    var englishIdx = $(evt.target).attr('data-english-idx');

    var definition = s[sentenceId].blocks[blockId].
                     definitions[pinyinIdx];
    var pinyin     = definition.pinyin;
    var english    = definition.english[englishIdx];

    console.log('select', pinyin, english);
    s[sentenceId].blocks[blockId].literal = english;
    s[sentenceId].blocks[blockId].dictIndex = pinyinIdx;
    saveCard(card);
  },
  // 'click .mandarin-continue': function () {
  //   console.log('continue');
  //   var card = activeCard();
  //   card.status = card.showAnswer ? 'missed' : 'perfect' ;
  //   saveCard(card);
  //   nextCard();
  // },
  'keydown .mandarin-input': function (evt) {
    if (evt.keyCode === 27) {
      var card = activeCard();
      card.showAnswer = true;
      saveCard(card);
      console.log('show answer')
      $(evt.target).popover({trigger: 'focus'});
      $(evt.target).popover('show');
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
      Meteor.call('postResponse', response);

      if (userAnswer === block.chinese) {
        $(evt.target).popover('destroy');
        markNextActiveBlock(card);
        card.showAnswer = false;
        saveCard(card);
      } else {
        console.log('incorrect', userAnswer, block.chinese);
        $(evt.currentTarget).select();
      }
    }
  }
});





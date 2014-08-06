// We fetch cards in the background without acting on them.
// This collection can be accessed without race conditions
// due to the singled threaded nature of JavaScript.
var cardsPrefetched = new Meteor.Collection(null);
var cards = new Meteor.Collection(null);

// FIXME: Move this somewhere else.
function empty(collection) {
  return !collection.findOne({});
}

fetchMoreCards = function(courseId) {
  Meteor.setTimeout(function() {
  Meteor.call('fetchCards', courseId, function (err, data) {
      console.log('Got data', data);
      instantiateCards(data);

      if ( empty(cards) ) {
        console.log('Cards immediately required.');
        for(var i=0; i<data.length; i++)
          cards.insert(data[i]);
        Session.set('activeCard', cards.findOne()._id);
      } else {
        console.log('Cards stored in prefetch cache.');
        for(var i=0; i<data.length; i++)
          cardsPrefetched.insert(data[i]);
      }
    });
  }, 1000);
}
// fetchMoreCards must have been called first.
activateNextCardSet = function (courseId) {
  if ( empty(cardsPrefetched) ) {
    // Cards are being loaded. Remove the cards we have now so that
    // when the new cards are fetched, they'll be used immediately.
    cards.remove({});
    console.log('Cards are being loaded.');
  } else {
    // Cards have been prefetched. Move them to the active collection.
    console.log('Cards available. Switching immediately.');
    cards.remove({});
    var lst = cardsPrefetched.find({}).fetch();
    for(var i=0; i<lst.length; i++)
      cards.insert(lst[i]);
    Session.set('activeCard', cards.findOne()._id);
    cardsPrefetched.remove({});
  }
}

// Instantiate extra card fields which are used for teaching logic.
function instantiateCards(cards) {
  for(var i=0; i<cards.length; i++) {
    cards[i].status = 'blocked';
    switch (cards[i].type) {
      case "chinese":
        instantiateChineseCard(cards[i]);
        break;
      default:
        console.log('instatiateCards: unhandled card type', cards[i].type);
        break;
    }
  }
  if( cards.length > 0)
    cards[0].status = 'active';
}

isLoadingCards = function () {
  return !cards.findOne({});
}
activeCard = function () {
  var activeId = Session.get('activeCard');
  return cards.findOne({_id: activeId});
}
saveCard = function (card) {
  cards.update({_id: card._id}, card);
}
nextCard = function () {
  var activeId = Session.get('activeCard');
  var newCard = cards.findOne({status: "blocked"});
  if ( !newCard ) {
    Session.set('activeCard', undefined);
    return;
  }
  newCard.status = 'active';
  saveCard(newCard);
  Session.set('activeCard', newCard._id);
}





// Templating related to study





Template.study.created = function () {
  console.log('created', this.data);
  cards.remove({});
  fetchMoreCards(this.data.courseId);
};
Template.study.rendered = function () {
  $("#study-nav").slimScroll({height: 'auto'});
  $(window).resize(function(){
    $("#study-nav").slimScroll({height: 'auto'});
  });
}
Template.study.cardTemplate = function () {
  if ( isLoadingCards() ) return 'studyLoading';
  var card = activeCard();
  if ( !card ) return 'studySprintCompleted';
  return 'studyMandarinCard';
}

Template.study.events({
  'click .study-show-answer': function () {
    var card = activeCard();
    card.showAnswer = true;
    saveCard(card);
  }
});


Template.studyProgress.cards = function() { return cards.find(); };


Template.studySprintCompleted.created = function () {
  // Prefetch cards when a sprint is completed.
  fetchMoreCards(this.data.courseId);
}
Template.studySprintCompleted.events({
  'click .sprint-continue': function () {
    activateNextCardSet(this.courseId);
  }
});

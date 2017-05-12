var staleMinutesSince, staleMsg, setStale, isStale, staleAt, fetchNew;

staleMinutesSince = function(d) {
  return Math.floor((Math.abs(new Date() - d)/1000)/60);
}

staleMsg = function(x) {
  return "<h1>Stale for " + x + " minutes.</h1>";
};

setStale = function(elem) {
  var staleFor;
  if (!isStale) {
    isStale = true;
    staleAt = new Date();
  }
  staleFor = staleMinutesSince(staleAt);
  if (staleFor >= 5) {
    elem.className = 'stale';
    elem.innerHTML = staleMsg(staleFor);
  }
  return elem;
}

fetchNew = function(){
  var xhr;
  xhr = new XMLHttpRequest();
  xhr.open('GET', window.location.href);
  xhr.responseType = "document";
  xhr.setRequestHeader('Accept', 'text/html');
  xhr.onload = function(){
    var elem = document.getElementsByTagName('body')[0];
    if (xhr.status !== 200) {
      return setStale(elem);
    }
    isStale = false;
    return elem.parentNode.replaceChild(this.responseXML.body, elem);
  };
  xhr.onerror = function() {
    var elem = document.getElementsByTagName('body')[0];
    return setStale(elem);
  };
  return xhr.send();
};
window.setInterval(fetchNew, 1000 * 120);

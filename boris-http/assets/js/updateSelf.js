var prettyDate, staleMsg, setStale, isStale, fetchNew;

prettyDate = function(d) {
  var months, hours, minutes, day, monthIndex, year;
  
  months = [
      "Jan"
    , "Feb"
    , "Mar"
    , "Apr"
    , "May"
    , "Jun"
    , "Jul"
    , "Aug"
    , "Sept"
    , "Oct"
    , "Nov"
    , "Dev"
  ];

  hours = d.getHours();
  minutes = d.getMinutes();
  day = d.getDate();
  monthIndex = d.getMonth();
  year = d.getFullYear();

  return hours + ':' + minutes + ', ' + day + ' ' + months[monthIndex] + ' ' + year;
}

staleMsg = function(t) {
  return "<h1 class=\"stale\"> Stale as of " + t + "</h1>";
};

setStale = function(elem) {
  if (!isStale) {
    isStale = true;
    return elem.innerHTML = staleMsg(prettyDate(new Date()));
  }
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


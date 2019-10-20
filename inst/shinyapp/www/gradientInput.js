// When a colour bar is dragged, send shiny the position
$(document).on('dragstop', '.ui-draggable', function(e, ui) {
  var left = ui.position.left;
  var module = $(this).closest('.draggables-module');
  var size = module.attr("data-shiny_size_corrected");
  var position = Math.round(left / size * 100);
  Shiny.setInputValue(e.target.id + '_percent', position, {priority: 'event'});
});

// When a colour bar is created, send shiny the position
$(document).on('dragcreate', '.ui-draggable', function(e, ui) {
  var left = e.target.offsetLeft - $(e.target).parent()[0].offsetLeft;
  var module = $(this).closest('.draggables-module');
  var size = module.attr("data-shiny_size_corrected");
  var position = Math.round(left / size * 100);
  Shiny.setInputValue(e.target.id + '_percent', position, {priority: 'event'});
});

// When clicking somewhere in the box, send shiny the position of the click
$(document).on('click', '.draggables-module', function(event) {
  if (!$(event.target).hasClass('draggables-module') &&
      !$(event.target).hasClass('draggables-container')) {
        return;
      }

  var module = $(this).closest('.draggables-module');
  var ns = module.attr('data-shiny_ns');
  var left = event.offsetX + 1;
  var percent = left / module.attr('data-shiny_size') * 100;

  Shiny.setInputValue(ns + 'add_drag_col', percent, {priority: 'event'});
});

// When the gradient input is resized (or initialized), tell Shiny about the new
// size
function GradientInputInitResize(ns) {
  var calculateNewSize = function() {
    var $parent = $("#" + ns + "draggables-box");
    var size = $parent[0].offsetWidth;
    $parent.attr("data-shiny_size", size);
    $parent.attr("data-shiny_size_corrected", size - 6);
    Shiny.setInputValue(ns + "gradient_resize", true, {priority: "event"});
  };

  calculateNewSize();
  new ResizeSensor($("#" + ns + "draggables-box")[0], function() {
    calculateNewSize();
  });
}

function GradientInputReposition(ns, data) {
  for (var row in data) {
    var id = ns + data[row].id + '-draggable';
    $("#" + id)[0].style.left = data[row].position + "%";
  }
}

// ResizeSensor - to detect when an element gets resized
function ResizeSensor(e,t){let l=parseInt(getComputedStyle(e));isNaN(l)&&(l=0),l--;let i=document.createElement("div");i.style.position="absolute",i.style.left="0px",i.style.top="0px",i.style.right="0px",i.style.bottom="0px",i.style.overflow="hidden",i.style.zIndex=l,i.style.visibility="hidden";let s=document.createElement("div");s.style.position="absolute",s.style.left="0px",s.style.top="0px",s.style.width="10000000px",s.style.height="10000000px",i.appendChild(s);let o=document.createElement("div");o.style.position="absolute",o.style.left="0px",o.style.top="0px",o.style.right="0px",o.style.bottom="0px",o.style.overflow="hidden",o.style.zIndex=l,o.style.visibility="hidden";let n=document.createElement("div");function d(){i.scrollLeft=1e7,i.scrollTop=1e7,o.scrollLeft=1e7,o.scrollTop=1e7}n.style.position="absolute",n.style.left="0px",n.style.top="0px",n.style.width="200%",n.style.height="200%",o.appendChild(n),e.appendChild(i),e.appendChild(o),d();let p=e.getBoundingClientRect(),y=p.width,h=p.height,c=function(){let l=e.getBoundingClientRect(),i=l.width,s=l.height;i==y&&s==h||(y=i,h=s,t()),d()};i.addEventListener("scroll",c),o.addEventListener("scroll",c)}

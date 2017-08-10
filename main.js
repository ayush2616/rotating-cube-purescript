var main=PS.Main;
var i=0;
function hello()
{
  i=i-0.1
  main.func()(i)();
}
var pre=0;
var state=false;
function mouseUp()
{
  state=false;
}
function mouseDown()
{
  state=true;
}
function mouseOut()
{
  state=false;
}
function moveFunc(e)
{
  if(state)
  {
    var x =e.clientX;
    var y = e.clientY;
    //main.funcX()((x)*0.05)(y*0.05)();
    pre=x;
  }
}
function clear(x)
{
  clearInterval(x);
}
main.startMouseHandlers()();
main.first()(0.5)();

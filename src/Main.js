"use strict"

exports.getTime=(new Date()).getTime();
var flag=true;
var speed=0.0;
var UpFlag=false;
exports.setFlag=function(state)
{
    flag=state;
    //console.log(flag);
    return flag;
};

exports.getFlag=function(){
    return flag;
}
exports.getSpeed=function()
{
    return speed;
}
exports.setSpeed=function(s)
{
    speed=s;
    return s;
}
exports.setUpFlag=function(state)
{
    UpFlag=state;
    //console.log(flag);
    return UpFlag;
};

exports.getUpFlag=function(){
    return true;
}
"use strict"

exports.getTime=(new Date()).getTime();
var flag=true;

exports.setFlag=function(state)
{
    flag=state;
    //console.log(flag);
    return flag;
};

exports.getFlag=function(){
    return flag;
}
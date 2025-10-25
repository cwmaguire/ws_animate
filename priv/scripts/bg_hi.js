'use strict';

console.log('Hi from worker');

self.addEventListener('message', msg);

function msg({data}){
  if(data == 'get data'){

    return true;
  }else{
    % store data
    return true
  }
}

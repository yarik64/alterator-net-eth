function ciFindElementByName(lst,name)
{
	for(var i =0;i<lst.length;++i)
		if (lst[i].getAttribute('name') == name)
			return lst[i];
	return null;
}

function ciTargetName(form)
{
    var inputs = form.getElementsByTagName('input');
    for(var i=0;i<inputs.length;++i)
	if ('name' == inputs[i].getAttribute('name'))
	    return inputs[i].getAttribute('value');

    return null;
}

function ciAdjustInputs(rinputs,pinputs)
{
	for(var i=0;i<rinputs.length;++i)
	{
		var input = rinputs[i];
		if (input.type == 'text' || input.type == 'hidden')
		{
			var nvalue = ciFindElementByName(pinputs,input.name);
			if (nvalue)
				input.defaultValue = input.value = nvalue.getAttribute('value');


		}
		else if (input.type == 'checkbox')
		{
			var nvalue = ciFindElementByName(pinputs,input.name);
			if (nvalue)//Note: there are no  hasAttribute function in IE
				input.defaultChecked = input.checked = nvalue.getAttribute('checked')?true:false;
		}
	}
}

function ciAdjustTextAreas(rinputs,pinputs)
{
	for(var i=0;i<rinputs.length;++i)
	{
		var input = rinputs[i];
		var nvalue = ciFindElementByName(pinputs,input.name);
		if (nvalue)
		   input.defaultValue = input.value = nodeText(nvalue);
	}
}

function ciAdjustSelects(rselects,pselects)
{

	function clearOptions(select)
	{
		while(select.length > 0)
			select.remove(select.length - 1);
	}
	
	function setupOptions(select,options)
	{
		for(var i = 0;i<options.length;++i)
		{
			var item = document.createElement('option');
			item.text = nodeText(options[i]);
			item.value = options[i].getAttribute('value');
			item.defaultSelected = item.selected = options[i].getAttribute('selected')?true:false;
			try{
				select.add(item,null);
			}catch(ex)
			{
				select.add(item);
			}
		}
	}
	
	for(var i = 0;i<rselects.length;++i)
	{
		var select = rselects[i];
		var nvalue = ciFindElementByName(pselects,select.name);
		if (!nvalue) continue;
		clearOptions(select);
		setupOptions(select,nvalue.getElementsByTagName('option'));
	}
}

function ciAdjustAnchors(anchors,target)
{
    if (!target) return;    

    var path=document.location.protocol +
             '//' +
	     document.location.host ;

    for(var i=0;i<anchors.length;++i)
    {
	var item = anchors[i];
	if ('alterator-ref2' == item.getAttribute('class'))
	    item.setAttribute('href', path + item.getAttribute('href2') + '/' + target);
    }
}

function ciAdjustSpans(rspans,pspans)
{
    // span arrays may be different (because of translation spans)
    var pi=0;
    for (var ri=0; ri<rspans.length; ri++){
      if ('alterator-label' == rspans[ri].getAttribute('class')){
        while (('alterator-label' != pspans[pi].getAttribute('class')) && (pi<pspans.length)) pi++
	rspans[ri].innerHTML = nodeText(pspans[pi]);
        pi++;
      }
    }
}

function netEthAdjust(page)
{
	//we assume that we only update field state here, no page changes
	var pforms = page.getElementsByTagName('form');
	var rforms = document.getElementsByTagName('form');

	if (pforms.length != rforms.length) return;

	for(var i=0;i<pforms.length;++i)
	{
		var current_pform = pforms[i];
		var current_rform = rforms[i];
		//select all normal inputs
		ciAdjustInputs(current_rform.getElementsByTagName('input'),
		                    current_pform.getElementsByTagName('input'));
		ciAdjustTextAreas(current_rform.getElementsByTagName('textarea'),
		                       current_pform.getElementsByTagName('textarea'));
		ciAdjustSelects(current_rform.getElementsByTagName('select'),
		                     current_pform.getElementsByTagName('select'));
		ciAdjustSpans(current_rform.getElementsByTagName('span'),
		                     current_pform.getElementsByTagName('span'));
		ciAdjustAnchors(current_rform.getElementsByTagName('a'),
		                    ciTargetName(current_pform));
	}
	
	document.body.style.cursor='default';
	document.forms[0].elements['name'].style.cursor='default';
}

function initNetEth()
{
	var has_async = makeXMLHttpRequest();
	if (!has_async) return;


	var form = document.getElementById('ajax-select');
	if (!form) return;

	var select = form.elements['name'];
	var button = form.elements['select'];

	if (!select || !button) return;

	addEvent(select,'change',function() {   document.body.style.cursor='wait';
						document.forms[0].elements['name'].style.cursor='wait';
						ajaxSubmit(netEthAdjust,'ajax-select','POST');});
	button.style.display='none';

	delete has_async;
}

//force loading without search
if (document.location.search)
	document.location = document.location.protocol + "//" + document.location.host + document.location.pathname;

addEvent(window,'load',initNetEth);

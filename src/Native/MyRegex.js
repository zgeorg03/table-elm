var _zgeorg03$table_elm$Native_MyRegex = function ()
{

	function safeRegex(raw){
		var res;

		try {
			res = _elm_lang$core$Result$Ok(new  RegExp(raw, 'g'));
		}catch (ex ){
			res = _elm_lang$core$Result$Err("could not convert string ");
		}

		return res;
	}

	return  {
		safeRegex : safeRegex 

	};
};

$(document).ready(function () {
    
    var allTokens = '';
    var tokenFrom = '';
    var tokenTo = '';
    var fromTo = '';
    var serverResponse = {};
    
    // queryBalance();
    // status();
    
    /** Open modal to choose token **/
    $('#select-token-from').on('click', function () {
        $('#select-token-modal').modal('show');
        removeLoader();
        fromTo = 'from';
        disableAlreadySelectedToken();
    });
    $('#select-token-to').on('click', function () {
        $('#select-token-modal').modal('show');
        removeLoader();
        fromTo = 'to';
        disableAlreadySelectedToken();
    });
    
    /** Search tokens **/
    $('#token-search-input').on('input change', function () {
        var searched = $(this).val().toLowerCase();
        $('.single-token').each(function () {
            if ($(this).find('.symbol').text().toLowerCase().indexOf(searched) > -1 || $(this).find('.name').text().toLowerCase().indexOf(searched) > -1) {
                $(this).css('display', 'flex');
            } else {
                $(this).css('display', 'none');
            }
        });
    })
    
    /** Try to enable button **/
    $('#token-amount-from, #token-amount-to').on('input', function () {
        enableTransaction();
    })
    $('#token-amount-from').on('input', function () {
        $('#token-amount-to').val($('#token-amount-from').val()*5);
    })
    $('#select-token-modal').on('hide.bs.modal', function (e) {
        fromTo = '';
        enableTransaction();
        $('#token-search-input').val('').trigger('change');
    })
    
    /** On token select in modal **/
    $(document).on('click', '.single-token', function () {
        var token = $(this).attr('data-symbol').toString();
        var chainId = $(this).attr('data-chain-id').toString();
        var results = $.map(allTokens, function(entry) {
            var match = entry.symbol.toString() === token && entry.chainId.toString() === chainId;
            return match ? entry : null;
        });
        switch (fromTo) {
            case 'from':
                tokenFrom = results[0];
                $('#select-token-from').addClass('selected').find('span').empty().html('' +
                    '<img src="'+tokenFrom.logoURI+'" alt="" />' +
                    tokenFrom.symbol+
                    '');
                break;
            case 'to':
                tokenTo = results[0];
                $('#select-token-to').addClass('selected').find('span').empty().html('' +
                    '<img src="'+tokenTo.logoURI+'" alt="" />' +
                    tokenTo.symbol+
                    '');
                break;
            default: break;
        }
        $('#select-token-modal').modal('hide');
    });
    
    
    /** Get JSON of the tokens **/
    $.getJSON( "assets/jsonResponse.json", function( data ) {
        serverResponse = data;
        // This is how we can get balance value from token A
        // $('#balance-token-from, #balance-value').text(serverResponse['cicCurrentState']['observableState']['Right']['contents']['getValue'][0][1][0][1]);
        
        // This is fake
        $('#balance-token-from, #balance-value').text(50000);
        
        
        
        // console.log(JSON.stringify(serverResponse));
        // console.log(serverResponse['cicCurrentState']['observableState']['Right']['contents']['getValue'][0][1][0][0]['unTokenName']);
        // console.log(serverResponse['cicCurrentState']['observableState']['Right']['contents']['getValue']);
        // alert(1);
    });
    /** Get JSON of the tokens **/
    $.getJSON( "assets/tokens.json", function( data ) {
        allTokens = data.tokens;
        var items = [];
        $.each( data.tokens, function( key, val ) {
            items.push( '' +
                '<div class="single-token" data-symbol="'+ val.symbol +'" data-chain-id="'+ val.chainId +'" data-address="'+ val.address +'">' +
                '    <img src="'+val.logoURI+'" alt="" />' +
                '    <div class="mr-auto">' +
                '        <h6 class="symbol">'+val.symbol+'</h6>' +
                '        <p class="name">'+val.name+'</p>' +
                '    </div>' +
                '    <div class="amount"><span class="loader"><svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M12 2C6.47715 2 2 6.47715 2 12C2 17.5228 6.47715 22 12 22C17.5228 22 22 17.5228 22 12C22 9.27455 20.9097 6.80375 19.1414 5" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"></path></svg></span></div>' +
                '</div>'+
            '');
        });
        
        $('#select-token-modal').find('.modal-body').empty().append(items);
    });
    
    
    /** Rotate tokens **/
    $('#rotate').on('click', function () {
        var fromEl = $('#select-token-from');
        var toEl = $('#select-token-to');
        var pom = '';
        
        pom = fromEl.html();
        fromEl.empty().html(toEl.html());
        toEl.empty().html(pom);
        
        if (fromEl.hasClass('selected') && !toEl.hasClass('selected')) {
            fromEl.removeClass('selected');
            toEl.addClass('selected');
        } else if (!fromEl.hasClass('selected') && toEl.hasClass('selected')) {
            fromEl.addClass('selected');
            toEl.removeClass('selected');
        }
        
        pom = tokenFrom;
        tokenFrom = tokenTo;
        tokenTo = pom;
        
        $('#token-amount-to').val('');
    });
    
    /** Set max value for exchange (same as balance) **/
    $('#max').on('click', function () {
        $('#token-amount-from').val(parseFloat($('#balance-token-from').text()));
        $('#token-amount-to').val($('#token-amount-from').val()*5);
    });
    
    /** Remove loader next ot the token **/
    function removeLoader() {
        var min = 0,
            max = 250;
        var rand = Math.floor(Math.random() * (max - min + 1) + min);
    
        if ($('.amount .loader').length) {
            $('.amount .loader').first().remove();
            setTimeout(removeLoader, rand );
        }
    }
    
    /** Disable already selected token **/
    function disableAlreadySelectedToken() {
        if (fromTo === 'from') {
            if ($('#select-token-to').hasClass('selected')) {
                disHlp1(tokenTo['address']);
            }
        } else if (fromTo === 'to') {
            if ($('#select-token-from').hasClass('selected')) {
                disHlp1(tokenFrom['address']);
            }
        }
    }
    function disHlp1(address) {
        $('.single-token').each(function () {
            if ($(this).attr('data-address').toString() === address.toString()) {
                $(this).addClass('disabled');
            } else {
                $(this).removeClass('disabled');
            }
        })
    }
    
    /** Is transaction ready **/
    function enableTransaction() {
        // $('#swap-button').attr('disabled',false).text('Swap');
        var amountFrom = $('#token-amount-from').val();
        var amountTo = $('#token-amount-to').val();
        if (amountFrom !== 0 && amountFrom !== '' && tokenFrom !== '' && tokenTo !== '') {
            $('#swap-button').attr('disabled',false).text('Swap');
        } else {
            $('#swap-button').prop("disabled", true).text('Enter an amount');
        }
    }
    
    /** Try to make transaction **/
    $('#swap-button').on('click', function () {
        swap();
    });
    
    
    
    var jsonExample =
        {
            "spAmountA": 1000,
            "spAmountB": 0,
            "spCoinB":
            {
                "cToken":
                {
                    "unTokenName": "A"
                },
                "cCurrency":
                {
                    "unCurrencySymbol":"6480bfa424253446eeb5de7d7f77287a675e3b84413be9d6f722cf80ff5cb0e6"
                }
            },
            "spCoinA":
            {
                "cToken":
                {
                    "unTokenName":""
                },
                "cCurrency":
                {
                    "unCurrencySymbol":""
                }
            }
        }
    var JSONPlaceholder ={"spAmountA":100,"spAmountB":0,"spCoinB":{"cToken":{"unTokenName":"A"},"cCurrency":{"unCurrencySymbol":"6480bfa424253446eeb5de7d7f77287a675e3b84413be9d6f722cf80ff5cb0e6"}},"spCoinA":{"cToken":{"unTokenName":""},"cCurrency":{"unCurrencySymbol":""}}}
    

    /** API **/
    
    function swap() {
        // JSONPlaceholder['spAmountA'] = $('#token-amount-from').val();
        // JSONPlaceholder['spCoinA']['cToken']['unTokenName'] = tokenFrom['name'];
        // JSONPlaceholder['spCoinA']['cCurrency']['unCurrencySymbol'] = tokenFrom['symbol'];
        // JSONPlaceholder['spAmountB'] = $('#token-amount-to').val();
        // JSONPlaceholder['spCoinB']['cToken']['unTokenName'] = tokenTo['name'];
        // JSONPlaceholder['spCoinB']['cCurrency']['unCurrencySymbol'] = tokenTo['symbol'];
        console.log('Swap');
        console.log(JSONPlaceholder);
        $.ajax({
            type: 'POST',
            data: JSON.stringify(JSONPlaceholder),
            dataType: "json",
            url: "http://109.93.254.15:8080/api/new/contract/instance/9ab02b1c-6a96-4ea8-8295-91fb731eb91f/endpoint/swap",
            contentType: "application/json",
            // processData: false,
            success: function (data) {
                console.log(JSON.stringify(data));
                console.log('Swap yes');
                
                // Search me to find balance calculation
                var blEl = $('#balance-token-from');
                blEl.text(parseInt(blEl.text()) - $('#token-amount-from').val());
                $('#balance-value').text(blEl.text());
                $('#token-amount-from, #token-amount-to').val('');
            },
            error: function(){
                console.log("Cannot get data0");
                console.log('Swap no');
            }
        });
    }
    
    function queryBalance() {
        $.ajax({
            type: 'POST',
            data: JSON.stringify([]),
            // dataType: "json",
            // crossDomain: true,
            // crossOrigin: true,
            url: 'http://109.93.254.15:8080/api/new/contract/instance/9ab02b1c-6a96-4ea8-8295-91fb731eb91f/endpoint/funds',
            // contentType: "application/json; charset=utf-8",
            dataType: "json",
            contentType: "application/json",
            // dataType: "text/plain",
            success: function (data) {
                // status();
                console.log(55551);
            },
            error: function(data){
                console.log(55552);
            }
        });
    }
    
    function status() {
        $.ajax({
            type: 'GET',
            url: "http://109.93.254.15:8080/api/new/contract/instance/9ab02b1c-6a96-4ea8-8295-91fb731eb91f/status",
            // url: "http://localhost:8080/api/new/contract/instance/8f314e28-e131-441c-9d2e-733bf323dee5/status",
            contentType: "application/json; charset=utf-8",
            // processData: false,
            // data: '{"foo":"bar"}',
            // dataType: 'json',
            // crossDomain: true,
            crossOrigin: true,
            success: function (data) {
                serverResponse = data;
                console.log(data);
                console.log(5555);
            },
            error: function(data){
                console.log('5555rfsfs');
                console.log(data);
                console.log("Cannot get data21");
            }
        });
    }
    
});

! ruhe — A UCI chess engine written in Fortran.
! Copyright (C) 2026 Michael White
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
!-
!! @brief Zobrist hash keys (781 orf them)
module ZobristM
  use, intrinsic :: iso_fortran_env, only: int64
  implicit none
  private

  ! Define the data type as int64
  integer, parameter, public :: &
    ZHK = int64

  ! Keys for piece type 'EMPTY'
  integer(kind=ZHK), dimension(0:63), parameter, private :: &
    ZHASH_EMPTY = 0_ZHK

  ! Keys for piece type 'BLACK'
  integer(kind=ZHK), dimension(0:63), parameter, private :: &
    ZHASH_BLACK = 0_ZHK

  ! Keys for piece type 'WP'
  integer(kind=ZHK), dimension(0:63), parameter, public :: &
    ZHASH_WP = &
    [ &
    int(z'E03810ACF99DD2CB', ZHK), &
    int(z'DCEDFBCDCBD9C9F6', ZHK), &
    int(z'2BCDBF9425E9C000', ZHK), &
    int(z'4A26A0334AC41E67', ZHK), &
    int(z'5992CDEFC90905CE', ZHK), &
    int(z'E7E13067F30A4949', ZHK), &
    int(z'C193703C5E6B78FB', ZHK), &
    int(z'C881E9E3CE92E75D', ZHK), &
    int(z'74ED470A82119CD5', ZHK), &
    int(z'5FF906D78D13C8CB', ZHK), &
    int(z'8488B511E5B4FB9C', ZHK), &
    int(z'F27243D7077EC7B7', ZHK), &
    int(z'374A45E5586DAF4B', ZHK), &
    int(z'EAD537AC179327D7', ZHK), &
    int(z'BABDBAF7644A97FA', ZHK), &
    int(z'97BAA309BD038837', ZHK), &
    int(z'A5F8AFE20C6D1834', ZHK), &
    int(z'C30C87EBC1DC8B38', ZHK), &
    int(z'F728652781CEF9A7', ZHK), &
    int(z'C59D04EB6FA79CD1', ZHK), &
    int(z'8E25D9E3A2667459', ZHK), &
    int(z'6C82AA44095C00BE', ZHK), &
    int(z'8B46CA66D85D981A', ZHK), &
    int(z'CEF32BB1ECF50070', ZHK), &
    int(z'873567502A4A71CA', ZHK), &
    int(z'A7A39AEC99ABD09D', ZHK), &
    int(z'388A1E327DB1A8A9', ZHK), &
    int(z'BC6AB34CAB36B2E0', ZHK), &
    int(z'929AC59A9577B833', ZHK), &
    int(z'2E6A5F293F8C0EDE', ZHK), &
    int(z'8319227E106BFDED', ZHK), &
    int(z'DDE10B02A2687C35', ZHK), &
    int(z'097AA2AA2CA6C768', ZHK), &
    int(z'BDB5A9D8B12FB924', ZHK), &
    int(z'AE82793E0648C47A', ZHK), &
    int(z'843F6B380E6F0ABD', ZHK), &
    int(z'4FFDBA263FED5950', ZHK), &
    int(z'ECFE63D065CF9D6E', ZHK), &
    int(z'C2DD394E2F533318', ZHK), &
    int(z'687B107BB5730529', ZHK), &
    int(z'C499DEB2759D1163', ZHK), &
    int(z'3D2132A5B731FA38', ZHK), &
    int(z'002BBF27E0ED1B6D', ZHK), &
    int(z'35BD607F1E4A1432', ZHK), &
    int(z'EE23C58CC160491D', ZHK), &
    int(z'789D1E53936E4E56', ZHK), &
    int(z'13CCB4934BA13763', ZHK), &
    int(z'43CC1FD01DF0E63A', ZHK), &
    int(z'830E0E48035517EF', ZHK), &
    int(z'CB73FF61754AB483', ZHK), &
    int(z'490D179D5EAA2FC5', ZHK), &
    int(z'36BDD5CEC1E92A6A', ZHK), &
    int(z'74B8A9B226B2DFE8', ZHK), &
    int(z'85CECC2E85FB47CB', ZHK), &
    int(z'A83081B543394233', ZHK), &
    int(z'18A95126E07F0BF4', ZHK), &
    int(z'BDEE96067DBC4CB7', ZHK), &
    int(z'DDBD30D37F2AD363', ZHK), &
    int(z'F1B2ADD0BE7C08D3', ZHK), &
    int(z'E4452B6BD3A28D54', ZHK), &
    int(z'CEE690942A6FCCF3', ZHK), &
    int(z'56E6854C1CA5AAAD', ZHK), &
    int(z'1C872F6408E88607', ZHK), &
    int(z'B08166307BFD0162', ZHK) &
    ]

  ! Keys for piece type 'BP'
  integer(kind=ZHK), dimension(0:63), parameter, public :: &
    ZHASH_BP = &
    [ &
    int(z'873CF4EB193B6BC8', ZHK), &
    int(z'0BFE2A7FE4D6D16E', ZHK), &
    int(z'18E11E0FBA25EA4E', ZHK), &
    int(z'E93A8A1E11EDCC78', ZHK), &
    int(z'5A1920D1FF4D15EC', ZHK), &
    int(z'7AD1A7A545705856', ZHK), &
    int(z'DD82D8456020A3DE', ZHK), &
    int(z'83CFB622B0266260', ZHK), &
    int(z'05B70902B2101244', ZHK), &
    int(z'46D753C446706B54', ZHK), &
    int(z'4B05C3057002FB90', ZHK), &
    int(z'B7ABD6B4F3A59F0E', ZHK), &
    int(z'FB6F3FF2FBF663B9', ZHK), &
    int(z'BA30743DBDCD5179', ZHK), &
    int(z'0AF1D4D5F0531567', ZHK), &
    int(z'BD9E33CCEA6C5C57', ZHK), &
    int(z'F8E839BB0F9D3F56', ZHK), &
    int(z'6BC5677EA08EDABA', ZHK), &
    int(z'DA4138B0DBE8FE27', ZHK), &
    int(z'4C0ED98AB1448F08', ZHK), &
    int(z'CC10D23F54A6C565', ZHK), &
    int(z'D49B575EF5A5EE6C', ZHK), &
    int(z'BBBE361DF0E7C932', ZHK), &
    int(z'8B38CAEA62A991AC', ZHK), &
    int(z'B6E2B18A7B79B833', ZHK), &
    int(z'D3EF32C8CFE65976', ZHK), &
    int(z'257D8020634C7938', ZHK), &
    int(z'7C26BA96FFB56323', ZHK), &
    int(z'7E0451479E17B82F', ZHK), &
    int(z'CEB71145DD79A5FD', ZHK), &
    int(z'731B51B9FEDA4BF4', ZHK), &
    int(z'C2C95883F987C724', ZHK), &
    int(z'A69F0991FA0ED82F', ZHK), &
    int(z'ACE69E42816FFE70', ZHK), &
    int(z'8650CA34E33665D9', ZHK), &
    int(z'65F6C19CB24AACBB', ZHK), &
    int(z'5E6B21F2FF6F746F', ZHK), &
    int(z'D7B324486A6D44AF', ZHK), &
    int(z'34FB45F24A1A59C0', ZHK), &
    int(z'D33416FDA6F58447', ZHK), &
    int(z'F89E0973DEA49F42', ZHK), &
    int(z'8C00DE0175F08DE9', ZHK), &
    int(z'712C3A6DE89E35C2', ZHK), &
    int(z'C858AEC320C974F8', ZHK), &
    int(z'51BEA9827D2DED53', ZHK), &
    int(z'92D56192FD8BF1E4', ZHK), &
    int(z'AACD81F1AFAE1AF2', ZHK), &
    int(z'0F3F848AA4173626', ZHK), &
    int(z'6BF5B2AFD63F839F', ZHK), &
    int(z'281B0D4438B26CD4', ZHK), &
    int(z'BBFEFB008E0207A9', ZHK), &
    int(z'3E76B6DD19A5C0C8', ZHK), &
    int(z'D3CECE94444CD004', ZHK), &
    int(z'DA91662E4D46A8A4', ZHK), &
    int(z'7E71254980092185', ZHK), &
    int(z'B3FFF27399CEE08D', ZHK), &
    int(z'A0F6376CE34AECD9', ZHK), &
    int(z'E8BF503DACE523DB', ZHK), &
    int(z'A103031487BACAB9', ZHK), &
    int(z'2115D0FAC89CEB37', ZHK), &
    int(z'B17933DE37CA3D65', ZHK), &
    int(z'90137224AE9FE609', ZHK), &
    int(z'C0D82BF9DAAEFDBE', ZHK), &
    int(z'F0C92825D9705BDE', ZHK) &
    ]

  ! Keys for piece type 'WN'
  integer(kind=ZHK), dimension(0:63), parameter, public :: &
    ZHASH_WN = &
    [ &
    int(z'4A3E05F71EFBD7ED', ZHK), &
    int(z'8D898206F04B2E82', ZHK), &
    int(z'2505FDC05B20939A', ZHK), &
    int(z'1517FA3AE734E9E8', ZHK), &
    int(z'2B25229A0E919113', ZHK), &
    int(z'2DBECD7620867B66', ZHK), &
    int(z'991A464D315E59C6', ZHK), &
    int(z'DF4E3535EF8E4314', ZHK), &
    int(z'4EB7F37003BEE1FB', ZHK), &
    int(z'0541566116E6FF5E', ZHK), &
    int(z'116DD44414C24580', ZHK), &
    int(z'5C2F8BF736054F11', ZHK), &
    int(z'E6AF71A395A8BAB8', ZHK), &
    int(z'F0A657F0764C32DF', ZHK), &
    int(z'57F77479C41DA553', ZHK), &
    int(z'1A65E793AEBA24EC', ZHK), &
    int(z'20FE114B95471CF2', ZHK), &
    int(z'ADDB207C5A523F34', ZHK), &
    int(z'B43FE8D52187E63B', ZHK), &
    int(z'477AC3E14B1901B9', ZHK), &
    int(z'18D904026C153105', ZHK), &
    int(z'2E617E9E40376290', ZHK), &
    int(z'45F7DF4C792ED7A2', ZHK), &
    int(z'DB43EED5029BD6AA', ZHK), &
    int(z'EC81BB15163175A0', ZHK), &
    int(z'628CA7414EFB28D7', ZHK), &
    int(z'4BD20B13C32E4E1B', ZHK), &
    int(z'73AC98BC923C7991', ZHK), &
    int(z'76B8B4AE78EBF25D', ZHK), &
    int(z'005333A387B8F58D', ZHK), &
    int(z'1385841BFC0A3482', ZHK), &
    int(z'934F71FBD3059448', ZHK), &
    int(z'FA2348B4DB6087E9', ZHK), &
    int(z'8F60D64DD4A2A70F', ZHK), &
    int(z'D1812CD5F4E3E869', ZHK), &
    int(z'EC73DBCFAA22BE95', ZHK), &
    int(z'5AA81558243C0EA9', ZHK), &
    int(z'1A214B7E749B5545', ZHK), &
    int(z'2676756880EB2313', ZHK), &
    int(z'5EAC062D7972E484', ZHK), &
    int(z'9CFA6FEDF5D0AD4F', ZHK), &
    int(z'66356DFB346E1E3B', ZHK), &
    int(z'733288574C348D97', ZHK), &
    int(z'FCCD44ADA20ED489', ZHK), &
    int(z'8B23FF97FFC26C8C', ZHK), &
    int(z'D2670539D3283651', ZHK), &
    int(z'973FA2BAA3C41791', ZHK), &
    int(z'785E6C07FAA390A3', ZHK), &
    int(z'1CDAEDF4968C0814', ZHK), &
    int(z'B363791176FD0F1D', ZHK), &
    int(z'28F52CB1B4032FEF', ZHK), &
    int(z'4F421203040ACC69', ZHK), &
    int(z'8A2883E9907E131D', ZHK), &
    int(z'E5010643F2EA02AE', ZHK), &
    int(z'2C2F533C2C40E131', ZHK), &
    int(z'ACE036FF53DEB6F5', ZHK), &
    int(z'DE9108D1036F5F88', ZHK), &
    int(z'B80533CBD49B7FDB', ZHK), &
    int(z'55010809BADC3315', ZHK), &
    int(z'10207A0E7C2E6738', ZHK), &
    int(z'44FB1F9CA6EA7046', ZHK), &
    int(z'1CDD0D1A8FF95DCC', ZHK), &
    int(z'B3CC2B834E0C4889', ZHK), &
    int(z'CB3BE4F57ADF8B22', ZHK) &
    ]

  ! Keys for piece type 'BN'
  integer(kind=ZHK), dimension(0:63), parameter, public :: &
    ZHASH_BN = &
    [ &
    int(z'A8637463D819E9C7', ZHK), &
    int(z'B33BA93599D8396F', ZHK), &
    int(z'55ADCB623C1352FD', ZHK), &
    int(z'3A31FDF70FC7A9A4', ZHK), &
    int(z'174241B738C86DA5', ZHK), &
    int(z'C96595D8DB6254FA', ZHK), &
    int(z'636DD77B06F5DE14', ZHK), &
    int(z'32429948FAC8E1C8', ZHK), &
    int(z'0AB6F5D1824B2FA1', ZHK), &
    int(z'4910ED0FF2261B90', ZHK), &
    int(z'D9F22E167E774700', ZHK), &
    int(z'3180F5838DD656DD', ZHK), &
    int(z'FBECACB18EF13F3B', ZHK), &
    int(z'7117311CAB1BA4BC', ZHK), &
    int(z'4623FF547B393F98', ZHK), &
    int(z'45CDFF53C25EBFD3', ZHK), &
    int(z'171647D52B6C4928', ZHK), &
    int(z'EFB09603AA901371', ZHK), &
    int(z'B46A599DD8AE26B8', ZHK), &
    int(z'8CC1914F5ED1ECD2', ZHK), &
    int(z'CA9FDB91C506B292', ZHK), &
    int(z'62042CAC9E852EE5', ZHK), &
    int(z'376C42E167467F9C', ZHK), &
    int(z'0ED675D12ACDBBFD', ZHK), &
    int(z'000AC4E975D630CE', ZHK), &
    int(z'3992C2A6B0F8F52F', ZHK), &
    int(z'21FD9B2EF2A74315', ZHK), &
    int(z'8A4C12D9A2E2CD6C', ZHK), &
    int(z'8AF329BE1B5B1BE4', ZHK), &
    int(z'118EA18B7B19936E', ZHK), &
    int(z'49804CC3D92E50DC', ZHK), &
    int(z'45ACBF275E936BA7', ZHK), &
    int(z'406F9A29E8B4F22D', ZHK), &
    int(z'C7C420D8BC955599', ZHK), &
    int(z'AFC4CD46E058A40E', ZHK), &
    int(z'F7C10E29BF5D31E3', ZHK), &
    int(z'F772F3E3B3317A0E', ZHK), &
    int(z'4886389ED242830F', ZHK), &
    int(z'676DE4935688CDD0', ZHK), &
    int(z'BCEACDC92E61BC91', ZHK), &
    int(z'AD1FE6C30F25AAA3', ZHK), &
    int(z'E9CF6C54081736E8', ZHK), &
    int(z'1FA38BEFB37725C1', ZHK), &
    int(z'B5A5B3ABFE6B3090', ZHK), &
    int(z'E8D79B5A5B804603', ZHK), &
    int(z'259D6841B5831474', ZHK), &
    int(z'0D518AD01242AEFE', ZHK), &
    int(z'11062F9701E6F0B3', ZHK), &
    int(z'E39F65985D42D9B4', ZHK), &
    int(z'634FD96DACFD652E', ZHK), &
    int(z'88AE8229206EA164', ZHK), &
    int(z'4BF9892A1004E770', ZHK), &
    int(z'471DB6B8E0790907', ZHK), &
    int(z'801E2252C6F2FFDA', ZHK), &
    int(z'525B61407F9B2A2C', ZHK), &
    int(z'04E1F277B88BF831', ZHK), &
    int(z'1E207FFAF6CE7FC6', ZHK), &
    int(z'23897F6D42CC60D6', ZHK), &
    int(z'8E2EF49EA2C493D2', ZHK), &
    int(z'00F2EF0D80DDFE39', ZHK), &
    int(z'8ED19704A7A90A7C', ZHK), &
    int(z'15E77AB4799A1B9B', ZHK), &
    int(z'EA49CD7F92521047', ZHK), &
    int(z'4DA2CE49B7DE5475', ZHK) &
    ]

  ! Keys for piece type 'WB'
  integer(kind=ZHK), dimension(0:63), parameter, public :: &
    ZHASH_WB = &
    [ &
    int(z'F37A984A3FAA2152', ZHK), &
    int(z'7AFF22AB5DF6669D', ZHK), &
    int(z'366FADC3A051202F', ZHK), &
    int(z'CC0F53B200E12400', ZHK), &
    int(z'0F20CB4EB2CEA3B7', ZHK), &
    int(z'49EE0639D52EAB9C', ZHK), &
    int(z'DC69840E752F4D03', ZHK), &
    int(z'32AC70BA8B01693B', ZHK), &
    int(z'C64ECC4F91B3EA43', ZHK), &
    int(z'D764B0379AD6809C', ZHK), &
    int(z'8802E2CDEE186830', ZHK), &
    int(z'E63E3C3BFBF6723B', ZHK), &
    int(z'417E42A56D20418F', ZHK), &
    int(z'22F106E2788CFFD9', ZHK), &
    int(z'9E27A0E339ED696D', ZHK), &
    int(z'95C3DCEFBBAB52E7', ZHK), &
    int(z'5FE9DC8B4F685A58', ZHK), &
    int(z'CDDFB97804AE085B', ZHK), &
    int(z'8193B26F1C6B8079', ZHK), &
    int(z'D69591B12CE27313', ZHK), &
    int(z'21AF21F9106336A3', ZHK), &
    int(z'3AD86B9B83E61A10', ZHK), &
    int(z'D274DCD3E79151D0', ZHK), &
    int(z'847849A7781F60A0', ZHK), &
    int(z'B078C53791168ADA', ZHK), &
    int(z'AC4634C52FE08B51', ZHK), &
    int(z'CCB0781A92FC0CA4', ZHK), &
    int(z'00532D10A4772BEC', ZHK), &
    int(z'E1A28B6A562CD4E7', ZHK), &
    int(z'DAEC82DDFD5CEAEF', ZHK), &
    int(z'CABFDD0BEC23E80D', ZHK), &
    int(z'CB742FEFD7228479', ZHK), &
    int(z'4AD4355C55B3ECBE', ZHK), &
    int(z'E13F842257F6C789', ZHK), &
    int(z'B438EFA3F1581E5A', ZHK), &
    int(z'3BC4B0D5FC2E203A', ZHK), &
    int(z'522AFA82600565C5', ZHK), &
    int(z'A34F83C39EE4969A', ZHK), &
    int(z'4036422BFC3D4300', ZHK), &
    int(z'C964F0866746EAE6', ZHK), &
    int(z'58C6DB31D3DFFA9A', ZHK), &
    int(z'9FE0A202E46255BB', ZHK), &
    int(z'89BC6EECCCF91C77', ZHK), &
    int(z'F976D30B536FD444', ZHK), &
    int(z'4A045BE610535A4E', ZHK), &
    int(z'3503CF29E243F475', ZHK), &
    int(z'8975417279714F66', ZHK), &
    int(z'5EED4AD063F1BC51', ZHK), &
    int(z'0E8255F82649CC30', ZHK), &
    int(z'6B38133A80E73AC2', ZHK), &
    int(z'E687BC0FE51EF69E', ZHK), &
    int(z'DF85865DD10827DE', ZHK), &
    int(z'A784D2DA145C99B1', ZHK), &
    int(z'18A9C14E0D444FAC', ZHK), &
    int(z'2F614C7D269DFF98', ZHK), &
    int(z'CC1200CEE129EA4C', ZHK), &
    int(z'F0CFA5F79E647D4B', ZHK), &
    int(z'70E9D6534DC41064', ZHK), &
    int(z'DA125796A938A48F', ZHK), &
    int(z'6213B7A3CA7A235A', ZHK), &
    int(z'B87D47EA7370B4E0', ZHK), &
    int(z'6C87FD87686A8E7D', ZHK), &
    int(z'BA8567912BA0BE1E', ZHK), &
    int(z'25EA6DB9938CB6B9', ZHK) &
    ]

  ! Keys for piece type 'BB'
  integer(kind=ZHK), dimension(0:63), parameter, public :: &
    ZHASH_BB = &
    [ &
    int(z'611DD20B881B3B7A', ZHK), &
    int(z'FDAEDD0E61D2E0F5', ZHK), &
    int(z'8870EE278F8089EE', ZHK), &
    int(z'488D519F7C5D9014', ZHK), &
    int(z'5AFD3FE4409B2BEB', ZHK), &
    int(z'6818F16BB7F5E788', ZHK), &
    int(z'E477693B1DA9CA20', ZHK), &
    int(z'972235E18C692C49', ZHK), &
    int(z'20C4BABE5481EED6', ZHK), &
    int(z'B9E02AE0EA180329', ZHK), &
    int(z'C73194E098C08698', ZHK), &
    int(z'C93C08C351213199', ZHK), &
    int(z'DDD407908B026B05', ZHK), &
    int(z'CE76A0ACE32C2608', ZHK), &
    int(z'3C2C601C0336C44B', ZHK), &
    int(z'853FFA75F34B15EE', ZHK), &
    int(z'5EC4E6137A22FFBF', ZHK), &
    int(z'3FB3FD65254C9EF2', ZHK), &
    int(z'1CA35C0812FF976B', ZHK), &
    int(z'A0A78F70DB50144C', ZHK), &
    int(z'4F8D7BFB5D67373A', ZHK), &
    int(z'5E7B8965E0BA1EAF', ZHK), &
    int(z'BCF5C0914DFA8C3E', ZHK), &
    int(z'9635AC9D3D47F424', ZHK), &
    int(z'CE1B917C7EDF314B', ZHK), &
    int(z'8D8D2980168AB52C', ZHK), &
    int(z'33CE4629BAFD0B19', ZHK), &
    int(z'5D6AB6F9788A7DEF', ZHK), &
    int(z'384204335BC848A6', ZHK), &
    int(z'0F2101DEE1C73B3A', ZHK), &
    int(z'13AA4483FDADFC9F', ZHK), &
    int(z'3B2155BE28E8CED1', ZHK), &
    int(z'FF756851558E5050', ZHK), &
    int(z'C90047542D9B3296', ZHK), &
    int(z'25354675550D6E8C', ZHK), &
    int(z'A061478683FE9A99', ZHK), &
    int(z'DC1558C5FDFE8B31', ZHK), &
    int(z'E8B3566582C429C2', ZHK), &
    int(z'3611BADA6199FA07', ZHK), &
    int(z'ADCA7374512AA2C1', ZHK), &
    int(z'C96F694A925DE152', ZHK), &
    int(z'E3A77EE9A910AF61', ZHK), &
    int(z'B773AE15275471E4', ZHK), &
    int(z'DF0119C174A3C98E', ZHK), &
    int(z'A5EBE41E4CEF1BF2', ZHK), &
    int(z'51B28EA25FEC32F9', ZHK), &
    int(z'2117C93FC7B0E538', ZHK), &
    int(z'EBBA0329519CF59A', ZHK), &
    int(z'F568D0EF056C40D1', ZHK), &
    int(z'01C2374A05337E28', ZHK), &
    int(z'E910BD69ADCE7355', ZHK), &
    int(z'356CA40D18229D33', ZHK), &
    int(z'16D23E910401005C', ZHK), &
    int(z'DBB71015D0EB2494', ZHK), &
    int(z'AB0897F29595CFFF', ZHK), &
    int(z'3733EDE8BD098A27', ZHK), &
    int(z'FB2E0E4A202D758B', ZHK), &
    int(z'BFD09AAC5F354395', ZHK), &
    int(z'EA6E2EC81D1E42D9', ZHK), &
    int(z'FEF4ABF944E1B8AD', ZHK), &
    int(z'5227C20548E4E7FE', ZHK), &
    int(z'68C5DA05309F5DA8', ZHK), &
    int(z'E8A71BE0C96708ED', ZHK), &
    int(z'45BAB8FB5087C089', ZHK) &
    ]

  ! Keys for piece type 'WR'
  integer(kind=ZHK), dimension(0:63), parameter, public :: &
    ZHASH_WR = &
    [ &
    int(z'B26A4E945E51D37A', ZHK), &
    int(z'4553E7E104E3E79A', ZHK), &
    int(z'6C3F270FA648BE93', ZHK), &
    int(z'BA3138D77430463C', ZHK), &
    int(z'43F906FED89F2AA2', ZHK), &
    int(z'6025794CF2128B05', ZHK), &
    int(z'3BC7BB2391DAAA3B', ZHK), &
    int(z'68621E68761A34BD', ZHK), &
    int(z'6E5232073414A8D0', ZHK), &
    int(z'42B616504DDBCF08', ZHK), &
    int(z'00ED3C6004F26F79', ZHK), &
    int(z'CB4B1F734293802A', ZHK), &
    int(z'9B359D2F5DC2378A', ZHK), &
    int(z'55C80B8849FD05BD', ZHK), &
    int(z'A0D07DBCCEFEEDC6', ZHK), &
    int(z'3BBCB8715C9628B6', ZHK), &
    int(z'748AF23413682FAF', ZHK), &
    int(z'78664FFA760F2B6E', ZHK), &
    int(z'13CB230837BD2974', ZHK), &
    int(z'813AD98CEE2772C8', ZHK), &
    int(z'56116869A87EE803', ZHK), &
    int(z'FF2BB093AD9737C8', ZHK), &
    int(z'B2EE604DDCD86DB4', ZHK), &
    int(z'DC89AA41E67DE58F', ZHK), &
    int(z'3B7B9263BD705B7D', ZHK), &
    int(z'F8C8D654F5F69DD6', ZHK), &
    int(z'582913BAEC33BE52', ZHK), &
    int(z'5D05E8E68F0080D9', ZHK), &
    int(z'A06A414FDA0D2339', ZHK), &
    int(z'EF969E3D1C0F6EB2', ZHK), &
    int(z'1BE6D4C5AFA53BE1', ZHK), &
    int(z'78E5D878081BEAF9', ZHK), &
    int(z'A5C2608CD7388070', ZHK), &
    int(z'A6498BD372EB5F60', ZHK), &
    int(z'CF91397419629962', ZHK), &
    int(z'B2DFDD8DBD804438', ZHK), &
    int(z'FAA098E6567DB2A5', ZHK), &
    int(z'21C3FC84281883FC', ZHK), &
    int(z'B5292A8325912F9D', ZHK), &
    int(z'FE2F33339796A3D7', ZHK), &
    int(z'768119CCBCE858A8', ZHK), &
    int(z'1139D8AD36CD0999', ZHK), &
    int(z'D215FEC2AADF8786', ZHK), &
    int(z'80F365B3E984E3CB', ZHK), &
    int(z'8DD384B284F359D4', ZHK), &
    int(z'7F58D2AEC310CBC3', ZHK), &
    int(z'EF6AFE1B3DC2D964', ZHK), &
    int(z'E3AFD389F31E3298', ZHK), &
    int(z'C52FBBA3D9F6E134', ZHK), &
    int(z'5AD978A2F386C08B', ZHK), &
    int(z'45B79A9BF0980EBA', ZHK), &
    int(z'C5204AC685D72551', ZHK), &
    int(z'121C6B14A6D6D734', ZHK), &
    int(z'5283BE682EE842BC', ZHK), &
    int(z'AB6315A93BBF3356', ZHK), &
    int(z'2D81DF45D71BF935', ZHK), &
    int(z'34E364BD559E14A7', ZHK), &
    int(z'AC6CF234E998CFA8', ZHK), &
    int(z'EDD108BF25A4A0AD', ZHK), &
    int(z'5B79FEBC9E3FEB29', ZHK), &
    int(z'59D7F3AF17F6419D', ZHK), &
    int(z'02219835F688E7A9', ZHK), &
    int(z'5A23E44BF1281A39', ZHK), &
    int(z'3607EE3D121F6604', ZHK) &
    ]

  ! Keys for piece type 'BR'
  integer(kind=ZHK), dimension(0:63), parameter, public :: &
    ZHASH_BR = &
    [ &
    int(z'BD456C2C8912AA7D', ZHK), &
    int(z'88664584475168D7', ZHK), &
    int(z'38FCA96C27C1F4BC', ZHK), &
    int(z'859C5E28A5B0061D', ZHK), &
    int(z'815C9199AAA0E607', ZHK), &
    int(z'A59F1877DB77F212', ZHK), &
    int(z'6BD88F12708848F8', ZHK), &
    int(z'9DDDF0450EFA4581', ZHK), &
    int(z'074D40960789C836', ZHK), &
    int(z'B5B33EFDD7BC740B', ZHK), &
    int(z'891DD0059C484DAA', ZHK), &
    int(z'BEA5866973937EAE', ZHK), &
    int(z'655DD135F4B9DCC1', ZHK), &
    int(z'C12B3142F4F41794', ZHK), &
    int(z'5140F009560BFA4F', ZHK), &
    int(z'DE3978DC46610338', ZHK), &
    int(z'60DEDA55EC6BC10A', ZHK), &
    int(z'F81753FEECB2AA1A', ZHK), &
    int(z'E5B534F2CA44F0D3', ZHK), &
    int(z'0EFB34B694284715', ZHK), &
    int(z'F4EC3E6D291A53DD', ZHK), &
    int(z'6A5A5E277DB339BB', ZHK), &
    int(z'75757224EB94C67E', ZHK), &
    int(z'7A9A0F5AAA2B55A2', ZHK), &
    int(z'D58958F03CC9FDAD', ZHK), &
    int(z'472CB3DBE79F7651', ZHK), &
    int(z'B7F5734FE0F23F31', ZHK), &
    int(z'31A5BF3DFA26604E', ZHK), &
    int(z'57DCDF4406D9C7E7', ZHK), &
    int(z'AF09369B90298151', ZHK), &
    int(z'E6E7C9D263620FB9', ZHK), &
    int(z'FFB86379D3A9F26F', ZHK), &
    int(z'323A5A0A44443827', ZHK), &
    int(z'DF7909C64DBDA895', ZHK), &
    int(z'0E068505937C8C7A', ZHK), &
    int(z'3DEA493DC1A08376', ZHK), &
    int(z'45FD99A6683B568C', ZHK), &
    int(z'083EFDE34BC89C42', ZHK), &
    int(z'9BC931D92C9166AC', ZHK), &
    int(z'B85C3D96E9790DF3', ZHK), &
    int(z'A34CFCA31824369C', ZHK), &
    int(z'2AF8636904771D93', ZHK), &
    int(z'1CBF5031A4C559D4', ZHK), &
    int(z'51F6E044020BDC90', ZHK), &
    int(z'69FCED5BC70F191C', ZHK), &
    int(z'324C1AABAC6EAE4B', ZHK), &
    int(z'28AA251EED34897F', ZHK), &
    int(z'52481A907B24D71A', ZHK), &
    int(z'797F3FF1893764BA', ZHK), &
    int(z'BD0119ECE5A81100', ZHK), &
    int(z'ABBD74268284B110', ZHK), &
    int(z'D08E02364840DC86', ZHK), &
    int(z'73F9BB4B91C24799', ZHK), &
    int(z'2DE65D4E0ADF865A', ZHK), &
    int(z'AF655A3A6D7897F9', ZHK), &
    int(z'401EAD9A11145DDA', ZHK), &
    int(z'4413603B930920C3', ZHK), &
    int(z'F07AC7C7012CBA15', ZHK), &
    int(z'1822FDF588646187', ZHK), &
    int(z'2EB1E3F6B1DA2D2B', ZHK), &
    int(z'2C37D20B8C15A793', ZHK), &
    int(z'FDBAC2AC4B74226D', ZHK), &
    int(z'B06CE4EF6C4A424B', ZHK), &
    int(z'3500F546D00FF980', ZHK) &
    ]

  ! Keys for piece type 'WQ'
  integer(kind=ZHK), dimension(0:63), parameter, public :: &
    ZHASH_WQ = &
    [ &
    int(z'B4A81FFF52A25EE8', ZHK), &
    int(z'5C037833251A1139', ZHK), &
    int(z'795B9A649AD95685', ZHK), &
    int(z'1C99A2E7109F6B3B', ZHK), &
    int(z'31E5C7E0B1FBF30F', ZHK), &
    int(z'FF4270D2D775A859', ZHK), &
    int(z'1381224C31DD0F84', ZHK), &
    int(z'A4099B2987E68DBB', ZHK), &
    int(z'0EF3F3AACFC32A66', ZHK), &
    int(z'3204F26B47EF9BB4', ZHK), &
    int(z'A2F89DABA9A08B38', ZHK), &
    int(z'F2B899DB8450859B', ZHK), &
    int(z'B158E8F57112BCF9', ZHK), &
    int(z'8F16B3EFAFAFB518', ZHK), &
    int(z'0142C282C845FD06', ZHK), &
    int(z'FF9407CF07F6E969', ZHK), &
    int(z'1640612E5D330AA7', ZHK), &
    int(z'7B609EF41CD20B23', ZHK), &
    int(z'E02763AF6121CBD4', ZHK), &
    int(z'5C341C8EE0397801', ZHK), &
    int(z'71CCE646558697BB', ZHK), &
    int(z'1453035CC2AD0BD0', ZHK), &
    int(z'4B054077AC3E51C6', ZHK), &
    int(z'69E2F32165199932', ZHK), &
    int(z'83537371041189F3', ZHK), &
    int(z'3D2FBD9F541C565C', ZHK), &
    int(z'76DC5C73D38DA49E', ZHK), &
    int(z'585E581B3E26BE76', ZHK), &
    int(z'142C34AC407C9EB8', ZHK), &
    int(z'0D854406D720F61F', ZHK), &
    int(z'AA6B9EED31997663', ZHK), &
    int(z'3913A217B1733B0D', ZHK), &
    int(z'350EE0FEE0BFEB44', ZHK), &
    int(z'6BD8BE16D03A699F', ZHK), &
    int(z'8820F0B067BE433B', ZHK), &
    int(z'557BEADD353FF373', ZHK), &
    int(z'D18A14036CFC760F', ZHK), &
    int(z'F37910ACF724B5D9', ZHK), &
    int(z'7161B4C4141BB7A8', ZHK), &
    int(z'E8A57849D689DBF4', ZHK), &
    int(z'D51466E99B3457F0', ZHK), &
    int(z'0E31EDBD01F1EF62', ZHK), &
    int(z'B89734726C341D13', ZHK), &
    int(z'A76A2204F6A13CB8', ZHK), &
    int(z'CE1BFBF802E2C694', ZHK), &
    int(z'B9343EB7A4645FB3', ZHK), &
    int(z'1028305F68812822', ZHK), &
    int(z'2E21CA97B21C76BA', ZHK), &
    int(z'73B067504F8FA723', ZHK), &
    int(z'26F4BE3D11946B78', ZHK), &
    int(z'AA17562C7AFA433D', ZHK), &
    int(z'6119343201AEF03F', ZHK), &
    int(z'2C4723700E082E31', ZHK), &
    int(z'7641566632454273', ZHK), &
    int(z'15DD0658DD233800', ZHK), &
    int(z'35982BC3B3BCC95F', ZHK), &
    int(z'6EBEA875FE0BC736', ZHK), &
    int(z'664056E5FC359E52', ZHK), &
    int(z'855BC8A2C1FC3CB8', ZHK), &
    int(z'D6C5A3CDEFF2A6E0', ZHK), &
    int(z'2AA6573C053B85C8', ZHK), &
    int(z'989F427C5E1BF7F6', ZHK), &
    int(z'CDF8A6497B394CC7', ZHK), &
    int(z'7E3FB95F03132F3F', ZHK) &
    ]

  ! Keys for piece type 'BQ'
  integer(kind=ZHK), dimension(0:63), parameter, public :: &
    ZHASH_BQ = &
    [ &
    int(z'3AC9AFCACE53E039', ZHK), &
    int(z'ACDD074BC035AAE0', ZHK), &
    int(z'8833EAA72DA29203', ZHK), &
    int(z'EBFFEAE3CB4A823A', ZHK), &
    int(z'F584CB0B6F0764C8', ZHK), &
    int(z'3BD3C63389AFA99F', ZHK), &
    int(z'50AF7DB9544CB014', ZHK), &
    int(z'8A0D57F0B55941DB', ZHK), &
    int(z'EFE6BBF5E65ECD54', ZHK), &
    int(z'636BC915E28654B0', ZHK), &
    int(z'02DAB2D660B4DF26', ZHK), &
    int(z'438E89BE8682DC58', ZHK), &
    int(z'A3644293514F9113', ZHK), &
    int(z'A21FD378FB94ACD0', ZHK), &
    int(z'F1E1E47CE4716FF0', ZHK), &
    int(z'69357902F990081C', ZHK), &
    int(z'819C9CAA966372EA', ZHK), &
    int(z'FD511EE7577AC1DB', ZHK), &
    int(z'D4BA8D8359070721', ZHK), &
    int(z'B718646584BFE97F', ZHK), &
    int(z'98AAFC3845644C6D', ZHK), &
    int(z'9223DA9B73B88B6C', ZHK), &
    int(z'0BB2601D20EFB45F', ZHK), &
    int(z'2B1C5B2D7DCA9F88', ZHK), &
    int(z'13FB00E14BD86297', ZHK), &
    int(z'6B057B098D4435A7', ZHK), &
    int(z'44A590B39DC01573', ZHK), &
    int(z'64984D26091B12DB', ZHK), &
    int(z'409C687E5BA23F8C', ZHK), &
    int(z'C7C427F50A076A79', ZHK), &
    int(z'D3511E8785B0E004', ZHK), &
    int(z'3879219571FA8C48', ZHK), &
    int(z'08115FC887A701A7', ZHK), &
    int(z'3038F2A4DA6E87CD', ZHK), &
    int(z'14A95CAEE34E2F05', ZHK), &
    int(z'F347758297E0DF81', ZHK), &
    int(z'27EC097F62178D6D', ZHK), &
    int(z'AED3557068771B15', ZHK), &
    int(z'30FD84464F4B794E', ZHK), &
    int(z'7C8D0A13CE7F5DA7', ZHK), &
    int(z'32D00334431CB767', ZHK), &
    int(z'1B6EB07FE9E0BD3F', ZHK), &
    int(z'821F0F822C7516E7', ZHK), &
    int(z'3187850823F9DE8A', ZHK), &
    int(z'3945616DCF8D4E2D', ZHK), &
    int(z'C36726213F4F6F7D', ZHK), &
    int(z'6A5A6F467A897749', ZHK), &
    int(z'78A8B49486E1D61A', ZHK), &
    int(z'7CF4E06259D79256', ZHK), &
    int(z'540E996DF05EAB78', ZHK), &
    int(z'15279959CEA216AE', ZHK), &
    int(z'C3540DF9F6C6EFA4', ZHK), &
    int(z'D13950A1B4C84FB3', ZHK), &
    int(z'B3DB2FC1E8DBBC73', ZHK), &
    int(z'ACD1A9489D0BB7DF', ZHK), &
    int(z'68323D7B029575BD', ZHK), &
    int(z'7E48B9DA7D526778', ZHK), &
    int(z'43992E3A6F0672F3', ZHK), &
    int(z'D81CD9CEE1974E3B', ZHK), &
    int(z'AA3301A52FB3A2DA', ZHK), &
    int(z'7C1FECA5BE5C4E6C', ZHK), &
    int(z'DC2C582267FBEE1F', ZHK), &
    int(z'2CA66242B20365E2', ZHK), &
    int(z'BAFEAE3136C8B094', ZHK) &
    ]

  ! Keys for piece type 'WK'
  integer(kind=ZHK), dimension(0:63), parameter, public :: &
    ZHASH_WK = &
    [ &
    int(z'C29EA83E6BC32FC1', ZHK), &
    int(z'21F6DE1191987418', ZHK), &
    int(z'E0BB1EBF2ABE4ADF', ZHK), &
    int(z'0D985F6151C19361', ZHK), &
    int(z'D332F449A5EE6FCF', ZHK), &
    int(z'17C6F26C1DBC3775', ZHK), &
    int(z'08C77B8712CE5F84', ZHK), &
    int(z'7F2DC15F69933CCC', ZHK), &
    int(z'39C1C218D0E2E6F8', ZHK), &
    int(z'D008AC31A5548461', ZHK), &
    int(z'FBE292BF6BD5CA5A', ZHK), &
    int(z'0A7178277375173A', ZHK), &
    int(z'F6853F48D10D33D9', ZHK), &
    int(z'881D0271A6DE32C7', ZHK), &
    int(z'63B4295E33FB51FC', ZHK), &
    int(z'B6ECDA764111968F', ZHK), &
    int(z'7CDB6F46F120806B', ZHK), &
    int(z'1D1B3DFEA3F6657F', ZHK), &
    int(z'2FF21C85F8077B3A', ZHK), &
    int(z'ACC30FA203BF3895', ZHK), &
    int(z'EF7902251161D0CF', ZHK), &
    int(z'48A639249DD181E9', ZHK), &
    int(z'644035B4B240D7FA', ZHK), &
    int(z'B96A7F3A800A1EAD', ZHK), &
    int(z'49A7CA2D837887F4', ZHK), &
    int(z'92EF44E199F80D74', ZHK), &
    int(z'2BA2838D5AECA866', ZHK), &
    int(z'3078D076979AB4BA', ZHK), &
    int(z'E47BF8E2CA82E1EB', ZHK), &
    int(z'8C90926A2F3BCF6B', ZHK), &
    int(z'8DCE0E6BC833CD48', ZHK), &
    int(z'3757357D69CB39B6', ZHK), &
    int(z'BA82CF0F472DBE43', ZHK), &
    int(z'85D09F14758244BE', ZHK), &
    int(z'B8A2A36FAD9A6D7A', ZHK), &
    int(z'488147D312792AFD', ZHK), &
    int(z'D4D72F8B68F58F38', ZHK), &
    int(z'F0731705E0C4FB78', ZHK), &
    int(z'DD5180DA0393E363', ZHK), &
    int(z'A49E1B5DBAD2153C', ZHK), &
    int(z'C519691A553A36BA', ZHK), &
    int(z'8E5181BE0679BFBB', ZHK), &
    int(z'1468AD7D51E6316C', ZHK), &
    int(z'0034612CFDC96235', ZHK), &
    int(z'D763A0C7740CAD69', ZHK), &
    int(z'C3BF84F5B1EA5DB2', ZHK), &
    int(z'9556A622FFB0B708', ZHK), &
    int(z'B12D6AE865B499D6', ZHK), &
    int(z'BC76E6594CC008B4', ZHK), &
    int(z'0C366E92CEF7EBBD', ZHK), &
    int(z'FF03FDF126D48441', ZHK), &
    int(z'D6C4571AB3F41116', ZHK), &
    int(z'52F98DAC13146D19', ZHK), &
    int(z'1590DCA30066C61B', ZHK), &
    int(z'842994F59034FB00', ZHK), &
    int(z'1015BFC468376785', ZHK), &
    int(z'573C16B5DBB042FA', ZHK), &
    int(z'D963E024AED1C7F6', ZHK), &
    int(z'0C101D37A0B8F684', ZHK), &
    int(z'C318DB1AD56EA33A', ZHK), &
    int(z'116C92190F00544D', ZHK), &
    int(z'60FF1171BE23DA6E', ZHK), &
    int(z'DC43C15CF66AC679', ZHK), &
    int(z'D8B94878DA1F722F', ZHK) &
    ]

  ! Keys for piece type 'BK'
  integer(kind=ZHK), dimension(0:63), parameter, public :: &
    ZHASH_BK = &
    [ &
    int(z'CAA6BEDA4124B6B9', ZHK), &
    int(z'07EE1F9D5BE840E0', ZHK), &
    int(z'55BD9927E42A3B73', ZHK), &
    int(z'155252E1D423E587', ZHK), &
    int(z'707AEC423E65BED4', ZHK), &
    int(z'27DD298FCA6D042B', ZHK), &
    int(z'CD09CEEAD2C925A1', ZHK), &
    int(z'4AA36B7169C4989C', ZHK), &
    int(z'43796A3911FF858D', ZHK), &
    int(z'73B0D161F1BD376F', ZHK), &
    int(z'4B06E71690BE846F', ZHK), &
    int(z'46E2494EEBBE66C6', ZHK), &
    int(z'CE8280F60E348664', ZHK), &
    int(z'FA46A4AC6E5AE743', ZHK), &
    int(z'20AAA6E8762D9AD8', ZHK), &
    int(z'1C1224BBB97F6060', ZHK), &
    int(z'981DBA0048DEB88E', ZHK), &
    int(z'821508686EACA4F6', ZHK), &
    int(z'4FCC930BF1C891A1', ZHK), &
    int(z'255DC325CB399C2D', ZHK), &
    int(z'1BB6C0308E79494D', ZHK), &
    int(z'2927C934F5C0FF0F', ZHK), &
    int(z'9661FC766E440044', ZHK), &
    int(z'597C7FAB4F4EDC7D', ZHK), &
    int(z'A8E940283ECC3821', ZHK), &
    int(z'23DE3021D9770F3F', ZHK), &
    int(z'F2835824527992F4', ZHK), &
    int(z'180BC589D49198A2', ZHK), &
    int(z'3E7F722756EDD5CC', ZHK), &
    int(z'9241188F314CBFE9', ZHK), &
    int(z'80ECBAF7EC311DAF', ZHK), &
    int(z'81A4EE84BC915314', ZHK), &
    int(z'3213E1C1053C86E5', ZHK), &
    int(z'681F0FB94A27ACC9', ZHK), &
    int(z'D46DD74A71A2EF69', ZHK), &
    int(z'4B3E946A5A7A709F', ZHK), &
    int(z'CA1DA8F6940583DB', ZHK), &
    int(z'B75B0825A725D1E9', ZHK), &
    int(z'D0C662730DBE409C', ZHK), &
    int(z'5BC8C95FDDE77392', ZHK), &
    int(z'FB4DAD7BBC70B8C0', ZHK), &
    int(z'F092BD04B754AABC', ZHK), &
    int(z'F635192DB37F1722', ZHK), &
    int(z'6494176FA74AB9DB', ZHK), &
    int(z'32440525F0429628', ZHK), &
    int(z'938385843E33E5B0', ZHK), &
    int(z'F78794D0D532B1A0', ZHK), &
    int(z'0FAEA74EF03BA62D', ZHK), &
    int(z'CD3A83C7CB10A198', ZHK), &
    int(z'6D6DDB992903C6E1', ZHK), &
    int(z'B5A7E1C352C2ABBF', ZHK), &
    int(z'186356C36DFD50AD', ZHK), &
    int(z'BABCA89EAE749D2F', ZHK), &
    int(z'EB9C7BAA6FF192E4', ZHK), &
    int(z'3C3EC4977663817C', ZHK), &
    int(z'FD410D0B47F2A262', ZHK), &
    int(z'339326CF22656BBA', ZHK), &
    int(z'CDAE9B7149B201C3', ZHK), &
    int(z'E85165AC51DE6526', ZHK), &
    int(z'0DB1FA54713329FC', ZHK), &
    int(z'69CD8480884E1946', ZHK), &
    int(z'EFE872699F2111C9', ZHK), &
    int(z'AD05DA0F37FF014F', ZHK), &
    int(z'4404F49DEB6E389F', ZHK) &
    ]

  ! Assemble 2D array from 1D arrays
  integer(kind=ZHK), dimension(0:13, 0:63), parameter, public :: &
    ZHASH_PST = transpose(reshape([ &
                                  ZHASH_EMPTY, ZHASH_BLACK, &
                                  ZHASH_WP, ZHASH_BP, &
                                  ZHASH_WN, ZHASH_BN, &
                                  ZHASH_WB, ZHASH_BB, &
                                  ZHASH_WR, ZHASH_BR, &
                                  ZHASH_WQ, ZHASH_BQ, &
                                  ZHASH_WK, ZHASH_BK], shape=[64, 14]))

  ! Keys for each of the castling rights -KQkq
  integer(kind=ZHK), parameter, public :: &
    ZH1 = int(z'1020EDC3CAA3EF8D', ZHK), &
    ZA1 = int(z'AAABBBFDD58CD182', ZHK), &
    ZH8 = int(z'74EEA945DF1723D8', ZHK), &
    ZA8 = int(z'F79F897221EB71E5', ZHK)

  integer(kind=ZHK), dimension(0:15), parameter, public :: &
    ZHASH_CASTLING = &
    [ &
    0_ZHK, &                                     ! .     .   .     .
    ZH1, & ! .     .   .     O-O
    ZA1, & ! .     .   O-O-O .
    ieor(ZA1, ZH1), & ! .     .   O-O-O O-O
    ZH8, & ! .     o-o .     .
    ieor(ZH8, ZH1), & ! .     o-o .     O-O
    ieor(ZH8, ZA1), & ! .     o-o O-O-O .
    ieor(ZH8, ieor(ZA1, ZH1)), & ! .     o-o O-O-O O-O
    ZA8, & ! o-o-o .   .     .
    ieor(ZA8, ZH1), & ! o-o-o .   .     O-O
    ieor(ZA8, ZA1), & ! o-o-o .   O-O-O .
    ieor(ZA8, ieor(ZA1, ZH1)), & ! o-o-o .   O-O-O O-O
    ieor(ZA8, ZH8), & ! o-o-o o-o .     .
    ieor(ZA8, ieor(ZH8, ZH1)), & ! o-o-o o-o .     O-O
    ieor(ieor(ZA8, ZH8), ZA1), & ! o-o-o o-o O-O-O .
    ieor(ieor(ZA8, ZH8), ieor(ZA1, ZH1)) & ! o-o-o o-o O-O-O O-O
    ]

  ! Keys for each of 8 en-passant file columns
  integer(kind=ZHK), dimension(0:7), parameter, public :: &
    ZHASH_EP_FILE = [ &
    int(z'FFB2DD88CC6A51C9', ZHK), &
    int(z'6036D7874462ADA6', ZHK), &
    int(z'8CFE6EC2FFEC9DBC', ZHK), &
    int(z'AD56007E32415B0C', ZHK), &
    int(z'E881EEC3D2F522BD', ZHK), &
    int(z'A5739B256AC4E439', ZHK), &
    int(z'0CAEA68469CB088A', ZHK), &
    int(z'9B1E7BA320477F3B', ZHK) &
    ]

  ! Key to flip when the side to move is Black
  integer(kind=ZHK), parameter, public :: &
    ZHASH_TURN = &
    int(z'3A1F67BA29B6D68F', ZHK)

  public :: &
    zhash_to_hexstr

contains

  !! @brief format string as 16 hex digits, leading zero padded
  function zhash_to_hexstr(zhash) result(str)

    integer(kind=ZHK), intent(in) :: zhash
    character(len=16) :: str

    write (str, fmt='(z16.16)') zhash
  end function zhash_to_hexstr

end module ZobristM
